use std::collections::{HashMap, HashSet};
use std::fs::{File, OpenOptions};
use std::io::{self, BufReader, BufWriter, Cursor, Read, Seek, SeekFrom, Write};
use std::path::Path;
use std::sync::{Arc, atomic::{AtomicBool, Ordering}};
use std::time::{SystemTime, UNIX_EPOCH};
use parking_lot::{Mutex, RwLock};
use memmap2::{MmapMut, MmapOptions};
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use uuid::Uuid;
use crc::{Crc, CRC_32_ISO_HDLC};
use lru::LruCache;
use snappy;
use futures::future::Future;
use serde::{Deserialize, Serialize};
use async_trait::AsyncTrait;
use mdns_sd::{ServiceDaemon, ServiceInfo};

const MAGIC: [u8; 8] = [0x55, 0xAA, 0xFE, 0xED, 0xFA, 0xCE, 0xDA, 0x7A];
const DEFAULT_PAGE_RAW_SIZE: u64 = 8192;
const DEFAULT_PAGE_HEADER_SIZE: u64 = 32;
const FREE_LIST_HEADER_SIZE: u64 = 12;
const FREE_LIST_ENTRIES_PER_PAGE: usize = ((DEFAULT_PAGE_RAW_SIZE - DEFAULT_PAGE_HEADER_SIZE - FREE_LIST_HEADER_SIZE) / 8) as usize;
const DEFAULT_MAX_DB_SIZE: u64 = 8000 * 1024 * 1024 * 1024;
const DEFAULT_MAX_PAGES: i64 = i64::MAX;
const DEFAULT_MAX_DOCUMENT_SIZE: u64 = 256 * 1024 * 1024;
const BATCH_GROW_PAGES: u64 = 16;
const DEFAULT_PAGE_CACHE_SIZE: usize = 2048;
const DEFAULT_VERSIONS_TO_KEEP: i32 = 2;
const MAX_CONSECUTIVE_EMPTY_FREE_LIST: u64 = 5;

const FLAG_DATA_PAGE: u8 = 0b00000001;
const FLAG_TRIE_PAGE: u8 = 0b00000010;
const FLAG_FREE_LIST_PAGE: u8 = 0b00000100;
const FLAG_INDEX_PAGE: u8 = 0b00001000;

#[derive(Clone, Copy, Debug)]
struct VersionedLink {
    page_id: i64,
    version: i32,
}

#[derive(Debug)]
struct DatabaseHeader {
    magic: [u8; 8],
    index_root: VersionedLink,
    path_lookup_root: VersionedLink,
    free_list_root: VersionedLink,
}

#[derive(Debug, Clone)]
struct PageHeader {
    crc: u32,
    version: i32,
    prev_page_id: i64,
    next_page_id: i64,
    flags: u8,
    data_length: i32,
    padding: [u8; 3],
}

#[derive(Debug)]
struct FreeListPage {
    next_free_list_page: i64,
    used_entries: i32,
    free_page_ids: Vec<i64>,
}

#[derive(Debug)]
struct ReverseTrieNode {
    edge: String,
    parent_index: i64,
    self_index: i64,
    document_id: Option<Uuid>,
    children: HashMap<char, i64>,
}

#[derive(Clone, Debug)]
struct Document {
    id: Uuid,
    first_page_id: i64,
    current_version: i32,
    paths: Vec<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Config {
    page_size: u64,
    page_header_size: u64,
    max_db_size: u64,
    max_pages: i64,
    max_document_size: u64,
    page_cache_size: usize,
    versions_to_keep: i32,
    use_mmap: bool,
    use_compression: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            page_size: DEFAULT_PAGE_RAW_SIZE,
            page_header_size: DEFAULT_PAGE_HEADER_SIZE,
            max_db_size: DEFAULT_MAX_DB_SIZE,
            max_pages: DEFAULT_MAX_PAGES,
            max_document_size: DEFAULT_MAX_DOCUMENT_SIZE,
            page_cache_size: DEFAULT_PAGE_CACHE_SIZE,
            versions_to_keep: DEFAULT_VERSIONS_TO_KEEP,
            use_mmap: true,
            use_compression: true,
        }
    }
}

#[derive(Debug, Default)]
pub struct CacheStats {
    hits: usize,
    misses: usize,
}

#[derive(Debug)]
pub enum StreamDbError {
    IoError(io::Error),
    InvalidData(String),
    NotFound(String),
    PageAllocationFailed,
    TrieCorruption,
}

impl From<io::Error> for StreamDbError {
    fn from(err: io::Error) -> Self {
        StreamDbError::IoError(err)
    }
}

pub trait Database {
    fn write_document(&mut self, path: &str, data: &mut dyn Read) -> Result<Uuid, StreamDbError>;
    fn get(&self, path: &str) -> Result<Vec<u8>, StreamDbError>;
    fn get_quick(&self, path: &str, quick: bool) -> Result<Vec<u8>, StreamDbError>;
    fn get_id_by_path(&self, path: &str) -> Result<Option<Uuid>, StreamDbError>;
    fn delete(&mut self, path: &str) -> Result<(), StreamDbError>;
    fn delete_by_id(&mut self, id: Uuid) -> Result<(), StreamDbError>;
    fn bind_to_path(&mut self, id: Uuid, path: &str) -> Result<(), StreamDbError>;
    fn unbind_path(&mut self, id: Uuid, path: &str) -> Result<(), StreamDbError>;
    fn search(&self, prefix: &str) -> Result<Vec<String>, StreamDbError>;
    fn list_paths(&self, id: Uuid) -> Result<Vec<String>, StreamDbError>;
    fn flush(&self) -> Result<(), StreamDbError>;
    fn calculate_statistics(&self) -> Result<(i64, i64), StreamDbError>;
    fn set_quick_mode(&mut self, enabled: bool);
    fn snapshot(&self) -> Result<Self, StreamDbError> where Self: Sized;
    fn get_cache_stats(&self) -> Result<CacheStats, StreamDbError>;
    fn get_stream(&self, path: &str) -> Result<impl Iterator<Item = Result<Vec<u8>, StreamDbError>>, StreamDbError>;
    fn get_async(&self, path: &str) -> impl Future<Output = Result<Vec<u8>, StreamDbError>>;
}

pub trait DatabaseBackend {
    fn write_document(&mut self, data: &mut dyn Read) -> Result<Uuid, StreamDbError>;
    fn read_document(&self, id: Uuid) -> Result<Vec<u8>, StreamDbError>;
    fn read_document_quick(&self, id: Uuid, quick: bool) -> Result<Vec<u8>, StreamDbError>;
    fn delete_document(&mut self, id: Uuid) -> Result<(), StreamDbError>;
    fn bind_path_to_document(&mut self, path: &str, id: Uuid) -> Result<Uuid, StreamDbError>;
    fn get_document_id_by_path(&self, path: &str) -> Result<Uuid, StreamDbError>;
    fn search_paths(&self, prefix: &str) -> Result<Vec<String>, StreamDbError>;
    fn list_paths_for_document(&self, id: Uuid) -> Result<Vec<String>, StreamDbError>;
    fn count_free_pages(&self) -> Result<i64, StreamDbError>;
    fn get_info(&self, id: Uuid) -> Result<String, StreamDbError>;
    fn delete_paths_for_document(&mut self, id: Uuid) -> Result<(), StreamDbError>;
    fn remove_from_index(&mut self, id: Uuid) -> Result<(), StreamDbError>;
    fn get_cache_stats(&self) -> Result<CacheStats, StreamDbError>;
    fn get_stream(&self, id: Uuid) -> Result<impl Iterator<Item = Result<Vec<u8>, StreamDbError>>, StreamDbError>;
}

fn write_varint<W: Write>(writer: &mut W, mut value: u64) -> Result<(), StreamDbError> {
    loop {
        if value < 0x80 {
            writer.write_u8(value as u8)?;
            break;
        }
        writer.write_u8((value as u8 & 0x7F) | 0x80)?;
        value >>= 7;
    }
    Ok(())
}

fn read_varint<R: Read>(reader: &mut R) -> Result<u64, StreamDbError> {
    let mut value = 0u64;
    let mut shift = 0u32;
    loop {
        let byte = reader.read_u8()?;
        value |= ((byte & 0x7F) as u64) << shift;
        shift += 7;
        if byte & 0x80 == 0 {
            break;
        }
        if shift > 63 {
            return Err(StreamDbError::InvalidData("Varint too large".to_string()));
        }
    }
    Ok(value)
}

fn zigzag_encode(value: i64) -> u64 {
    ((value << 1) ^ (value >> 63)) as u64
}

fn zigzag_decode(value: u64) -> i64 {
    ((value >> 1) as i64) ^ -((value & 1) as i64)
}

struct FileBackend {
    file: Mutex<File>,
    mmap: RwLock<Option<MmapMut>>,
    config: Config,
    page_cache: Mutex<LruCache<i64, Vec<u8>>>,
    cache_stats: Mutex<CacheStats>,
    quick_mode: AtomicBool,
    trie_root: Mutex<VersionedLink>,
    free_list_root: Mutex<VersionedLink>,
    document_index_root: Mutex<VersionedLink>,
    old_versions: Mutex<HashMap<Uuid, Vec<(i32, i64)>>>,
    next_page_id: Mutex<i64>,
}

impl FileBackend {
    fn new<P: AsRef<Path>>(path: P, config: Config) -> Result<Self, StreamDbError> {
        let file = OpenOptions::new().read(true).write(true).create(true).open(path)?;
        let mmap = if config.use_mmap {
            Some(unsafe { MmapOptions::new().map_mut(&file)? })
        } else {
            None
        };
        Ok(Self {
            file: Mutex::new(file),
            mmap: RwLock::new(mmap),
            config,
            page_cache: Mutex::new(LruCache::new(config.page_cache_size)),
            cache_stats: Mutex::new(CacheStats::default()),
            quick_mode: AtomicBool::new(false),
            trie_root: Mutex::new(VersionedLink { page_id: -1, version: 0 }),
            free_list_root: Mutex::new(VersionedLink { page_id: -1, version: 0 }),
            document_index_root: Mutex::new(VersionedLink { page_id: -1, version: 0 }),
            old_versions: Mutex::new(HashMap::new()),
            next_page_id: Mutex::new(0),
        })
    }

    fn allocate_page(&mut self) -> Result<i64, StreamDbError> {
        let mut free_list_root = self.free_list_root.lock();
        if free_list_root.page_id != -1 {
            let mut buffer = vec![0u8; self.config.page_size as usize];
            let offset = free_list_root.page_id as u64 * self.config.page_size;
            {
                let mmap = self.mmap.read();
                if let Some(mmap) = mmap.as_ref() {
                    let start = offset as usize;
                    buffer.copy_from_slice(&mmap[start..start + self.config.page_size as usize]);
                } else {
                    let mut file = self.file.lock();
                    file.seek(SeekFrom::Start(offset))?;
                    file.read_exact(&mut buffer)?;
                }
            }
            let mut reader = Cursor::new(buffer);
            let header = self.read_page_header(free_list_root.page_id)?;
            reader.seek(SeekFrom::Start(self.config.page_header_size))?;
            let next_free_list_page = reader.read_i64::<LittleEndian>()?;
            let used_entries = reader.read_i32::<LittleEndian>()?;
            let mut free_page_ids = vec![];
            for _ in 0..used_entries {
                free_page_ids.push(reader.read_i64::<LittleEndian>()?);
            }
            if let Some(page_id) = free_page_ids.pop() {
                let new_used_entries = used_entries - 1;
                let mut writer = Cursor::new(vec![0u8; self.config.page_size as usize]);
                writer.write_all(&buffer[0..self.config.page_header_size as usize])?;
                writer.write_i64::<LittleEndian>(next_free_list_page)?;
                writer.write_i32::<LittleEndian>(new_used_entries)?;
                for id in &free_page_ids {
                    writer.write_i64::<LittleEndian>(*id)?;
                }
                self.write_raw_page(free_list_root.page_id, &writer.into_inner()[self.config.page_header_size as usize..], header.version)?;
                if new_used_entries == 0 {
                    free_list_root.page_id = next_free_list_page;
                }
                return Ok(page_id);
            }
        }
        // Grow file
        let mut next_page_id = self.next_page_id.lock();
        let page_id = *next_page_id;
        *next_page_id += 1;
        let new_size = (page_id + BATCH_GROW_PAGES as i64) as u64 * self.config.page_size;
        if new_size > self.config.max_db_size {
            return Err(StreamDbError::PageAllocationFailed);
        }
        let mut file = self.file.lock();
        file.set_len(new_size)?;
        if let Some(mmap) = self.mmap.write().as_mut() {
            *mmap = unsafe { MmapOptions::new().len(new_size as usize).map_mut(&*file)? };
        }
        Ok(page_id)
    }

    fn read_page_header(&self, page_id: i64) -> Result<PageHeader, StreamDbError> {
        let offset = page_id as u64 * self.config.page_size;
        let mut buffer = vec![0u8; self.config.page_header_size as usize];
        {
            let mmap = self.mmap.read();
            if let Some(mmap) = mmap.as_ref() {
                let start = offset as usize;
                buffer.copy_from_slice(&mmap[start..start + self.config.page_header_size as usize]);
            } else {
                let mut file = self.file.lock();
                file.seek(SeekFrom::Start(offset))?;
                file.read_exact(&mut buffer)?;
            }
        }
        let mut reader = Cursor::new(buffer);
        let crc = reader.read_u32::<LittleEndian>()?;
        let version = reader.read_i32::<LittleEndian>()?;
        let prev_page_id = reader.read_i64::<LittleEndian>()?;
        let next_page_id = reader.read_i64::<LittleEndian>()?;
        let flags = reader.read_u8()?;
        let data_length = reader.read_i32::<LittleEndian>()?;
        let mut padding = [0u8; 3];
        reader.read_exact(&mut padding)?;
        Ok(PageHeader {
            crc,
            version,
            prev_page_id,
            next_page_id,
            flags,
            data_length,
            padding,
        })
    }

    fn write_raw_page(&self, page_id: i64, data: &[u8], version: i32) -> Result<(), StreamDbError> {
        let offset = page_id as u64 * self.config.page_size;
        let data_length = data.len() as i32;
        let mut buffer = vec![0u8; self.config.page_size as usize];
        let crc = Crc::<u32>::new(&CRC_32_ISO_HDLC).checksum(data);
        let mut writer = Cursor::new(&mut buffer);
        writer.write_u32::<LittleEndian>(crc)?;
        writer.write_i32::<LittleEndian>(version)?;
        writer.write_i64::<LittleEndian>(-1)?; // prev_page_id
        writer.write_i64::<LittleEndian>(-1)?; // next_page_id
        writer.write_u8(FLAG_DATA_PAGE)?;
        writer.write_i32::<LittleEndian>(data_length)?;
        writer.write_all(&[0u8; 3])?; // padding
        writer.write_all(data)?;
        {
            let mmap = self.mmap.read();
            if let Some(mmap) = mmap.as_ref() {
                let start = offset as usize;
                mmap[start..start + self.config.page_size as usize].copy_from_slice(&buffer);
            } else {
                let mut file = self.file.lock();
                file.seek(SeekFrom::Start(offset))?;
                file.write_all(&buffer)?;
            }
        }
        Ok(())
    }

    fn read_raw_page(&self, page_id: i64) -> Result<Vec<u8>, StreamDbError> {
        let mut cache = self.page_cache.lock();
        if let Some(data) = cache.get(&page_id) {
            self.cache_stats.lock().hits += 1;
            return Ok(data.clone());
        }
        self.cache_stats.lock().misses += 1;
        let offset = page_id as u64 * self.config.page_size;
        let mut buffer = vec![0u8; self.config.page_size as usize];
        {
            let mmap = self.mmap.read();
            if let Some(mmap) = mmap.as_ref() {
                let start = offset as usize;
                buffer.copy_from_slice(&mmap[start..start + self.config.page_size as usize]);
            } else {
                let mut file = self.file.lock();
                file.seek(SeekFrom::Start(offset))?;
                file.read_exact(&mut buffer)?;
            }
        }
        let header = self.read_page_header(page_id)?;
        if !self.quick_mode.load(Ordering::Relaxed) {
            let crc = Crc::<u32>::new(&CRC_32_ISO_HDLC).checksum(&buffer[self.config.page_header_size as usize..]);
            if crc != header.crc {
                return Err(StreamDbError::InvalidData("CRC mismatch".to_string()));
            }
        }
        let data = buffer[self.config.page_header_size as usize..(self.config.page_header_size as usize + header.data_length as usize)].to_vec();
        cache.put(page_id, data.clone());
        Ok(data)
    }

    fn serialize_document(&self, document: &Document) -> Result<Vec<u8>, StreamDbError> {
        let mut buffer = Vec::new();
        buffer.write_all(document.id.as_bytes())?;
        buffer.write_i64::<LittleEndian>(document.first_page_id)?;
        buffer.write_i32::<LittleEndian>(document.current_version)?;
        write_varint(&mut buffer, document.paths.len() as u64)?;
        for path in &document.paths {
            let bytes = path.as_bytes();
            write_varint(&mut buffer, bytes.len() as u64)?;
            buffer.write_all(bytes)?;
        }
        if self.config.use_compression {
            Ok(snappy::compress(&buffer)?)
        } else {
            Ok(buffer)
        }
    }

    fn deserialize_document(&self, data: &[u8]) -> Result<Document, StreamDbError> {
        let decompressed = if self.config.use_compression {
            snappy::decompress(data)?
        } else {
            data.to_vec()
        };
        let mut reader = Cursor::new(decompressed);
        let mut id_bytes = [0u8; 16];
        reader.read_exact(&mut id_bytes)?;
        let id = Uuid::from_bytes(id_bytes);
        let first_page_id = reader.read_i64::<LittleEndian>()?;
        let current_version = reader.read_i32::<LittleEndian>()?;
        let path_count = read_varint(&mut reader)? as usize;
        let mut paths = Vec::with_capacity(path_count);
        for _ in 0..path_count {
            let path_len = read_varint(&mut reader)? as usize;
            let mut path_bytes = vec![0u8; path_len];
            reader.read_exact(&mut path_bytes)?;
            let path = String::from_utf8(path_bytes).map_err(|e| StreamDbError::InvalidData(e.to_string()))?;
            paths.push(path);
        }
        Ok(Document {
            id,
            first_page_id,
            current_version,
            paths,
        })
    }

    fn serialize_trie_node(&self, node: &ReverseTrieNode) -> Result<Vec<u8>, StreamDbError> {
        let mut buffer = Vec::new();
        let edge_bytes = node.edge.as_bytes();
        write_varint(&mut buffer, edge_bytes.len() as u64)?;
        buffer.write_all(edge_bytes)?;
        write_varint(&mut buffer, zigzag_encode(node.parent_index))?;
        write_varint(&mut buffer, zigzag_encode(node.self_index))?;
        if let Some(id) = node.document_id {
            buffer.write_u8(1)?;
            buffer.write_all(id.as_bytes())?;
        } else {
            buffer.write_u8(0)?;
        }
        write_varint(&mut buffer, node.children.len() as u64)?;
        for (&c, &index) in &node.children {
            write_varint(&mut buffer, c as u64)?;
            write_varint(&mut buffer, zigzag_encode(index))?;
        }
        if self.config.use_compression {
            Ok(snappy::compress(&buffer)?)
        } else {
            Ok(buffer)
        }
    }

    fn deserialize_trie_node(&self, data: &[u8]) -> Result<ReverseTrieNode, StreamDbError> {
        let decompressed = if self.config.use_compression {
            snappy::decompress(data)?
        } else {
            data.to_vec()
        };
        let mut reader = Cursor::new(decompressed);
        let edge_len = read_varint(&mut reader)? as usize;
        let mut edge_bytes = vec![0u8; edge_len];
        reader.read_exact(&mut edge_bytes)?;
        let edge = String::from_utf8(edge_bytes).map_err(|e| StreamDbError::InvalidData(e.to_string()))?;
        let parent_index = zigzag_decode(read_varint(&mut reader)?);
        let self_index = zigzag_decode(read_varint(&mut reader)?);
        let has_id = reader.read_u8()?;
        let document_id = if has_id == 1 {
            let mut bytes = [0u8; 16];
            reader.read_exact(&mut bytes)?;
            Some(Uuid::from_bytes(bytes))
        } else {
            None
        };
        let children_count = read_varint(&mut reader)? as usize;
        let mut children = HashMap::with_capacity(children_count);
        for _ in 0..children_count {
            let c = read_varint(&mut reader)? as u32;
            let c_char = char::try_from(c).map_err(|_| StreamDbError::InvalidData("Invalid child char".to_string()))?;
            let index = zigzag_decode(read_varint(&mut reader)?);
            children.insert(c_char, index);
        }
        Ok(ReverseTrieNode {
            edge,
            parent_index,
            self_index,
            document_id,
            children,
        })
    }

    fn trie_insert(&mut self, path: &str, id: Uuid) -> Result<(), StreamDbError> {
        let reversed: String = path.chars().rev().collect();
        let mut current_page_id = self.trie_root.lock().page_id;
        if current_page_id == -1 {
            current_page_id = self.allocate_page()?;
            self.trie_root.lock().page_id = current_page_id;
            let root_node = ReverseTrieNode {
                edge: String::new(),
                parent_index: -1,
                self_index: current_page_id,
                document_id: None,
                children: HashMap::new(),
            };
            let data = self.serialize_trie_node(&root_node)?;
            self.write_raw_page(current_page_id, &data, 1)?;
        }
        let mut current = self.deserialize_trie_node(&self.read_raw_page(current_page_id)?)?;
        let mut remaining = reversed.as_str();
        while !remaining.is_empty() {
            let first_char = remaining.chars().next().unwrap();
            if let Some(&child_id) = current.children.get(&first_char) {
                let child = self.deserialize_trie_node(&self.read_raw_page(child_id)?)?;
                let edge = child.edge.as_str();
                let common_len = edge.chars().zip(remaining.chars()).take_while(|(a, b)| a == b).count();
                if common_len < edge.len() {
                    // Split node
                    let new_child_page_id = self.allocate_page()?;
                    let new_child = ReverseTrieNode {
                        edge: edge[common_len..].to_string(),
                        parent_index: child.self_index,
                        self_index: new_child_page_id,
                        document_id: child.document_id,
                        children: child.children.clone(),
                    };
                    current.children.remove(&first_char);
                    current.children.insert(first_char, new_child_page_id);
                    current.document_id = None;
                    let new_edge = edge[0..common_len].to_string();
                    let new_node = ReverseTrieNode {
                        edge: new_edge.clone(),
                        parent_index: current.self_index,
                        self_index: child_id,
                        document_id: None,
                        children: HashMap::new(),
                    };
                    new_node.children.insert(edge.chars().nth(common_len).unwrap(), new_child_page_id);
                    self.write_raw_page(child_id, &self.serialize_trie_node(&new_node)?, child.version + 1)?;
                    self.write_raw_page(new_child_page_id, &self.serialize_trie_node(&new_child)?, 1)?;
                    remaining = &remaining[common_len..];
                    current = new_node;
                } else {
                    remaining = &remaining[edge.len()..];
                    current_page_id = child_id;
                    current = child;
                }
            } else {
                let new_page_id = self.allocate_page()?;
                let new_node = ReverseTrieNode {
                    edge: remaining.to_string(),
                    parent_index: current.self_index,
                    self_index: new_page_id,
                    document_id: Some(id),
                    children: HashMap::new(),
                };
                current.children.insert(first_char, new_page_id);
                self.write_raw_page(new_page_id, &self.serialize_trie_node(&new_node)?, 1)?;
                self.write_raw_page(current_page_id, &self.serialize_trie_node(&current)?, current.version + 1)?;
                return Ok(());
            }
        }
        current.document_id = Some(id);
        self.write_raw_page(current_page_id, &self.serialize_trie_node(&current)?, current.version + 1)?;
        Ok(())
    }
}

impl DatabaseBackend for FileBackend {
    fn write_document(&mut self, data: &mut dyn Read) -> Result<Uuid, StreamDbError> {
        let id = Uuid::new_v4();
        let mut first_page_id = -1;
        let mut prev_page_id = -1;
        let mut current_page_id = -1;
        let mut total_size = 0;
        let mut version = 1;
        loop {
            let mut buffer = vec![0u8; (self.config.page_size - self.config.page_header_size) as usize];
            let bytes_read = data.read(&mut buffer)?;
            if bytes_read == 0 {
                break;
            }
            total_size += bytes_read as u64;
            if total_size > self.config.max_document_size {
                return Err(StreamDbError::InvalidData("Document size exceeds limit".to_string()));
            }
            buffer.truncate(bytes_read);
            current_page_id = self.allocate_page()?;
            if first_page_id == -1 {
                first_page_id = current_page_id;
            }
            let header = PageHeader {
                crc: Crc::<u32>::new(&CRC_32_ISO_HDLC).checksum(&buffer),
                version,
                prev_page_id,
                next_page_id: -1,
                flags: FLAG_DATA_PAGE,
                data_length: bytes_read as i32,
                padding: [0u8; 3],
            };
            let mut page_buffer = vec![0u8; self.config.page_size as usize];
            let mut writer = Cursor::new(&mut page_buffer);
            writer.write_u32::<LittleEndian>(header.crc)?;
            writer.write_i32::<LittleEndian>(header.version)?;
            writer.write_i64::<LittleEndian>(header.prev_page_id)?;
            writer.write_i64::<LittleEndian>(header.next_page_id)?;
            writer.write_u8(header.flags)?;
            writer.write_i32::<LittleEndian>(header.data_length)?;
            writer.write_all(&header.padding)?;
            writer.write_all(&buffer)?;
            self.write_raw_page(current_page_id, &buffer, version)?;
            if prev_page_id != -1 {
                let mut prev_header = self.read_page_header(prev_page_id)?;
                prev_header.next_page_id = current_page_id;
                let prev_data = self.read_raw_page(prev_page_id)?;
                let mut prev_buffer = vec![0u8; self.config.page_size as usize];
                let mut prev_writer = Cursor::new(&mut prev_buffer);
                prev_writer.write_u32::<LittleEndian>(prev_header.crc)?;
                prev_writer.write_i32::<LittleEndian>(prev_header.version)?;
                prev_writer.write_i64::<LittleEndian>(prev_header.prev_page_id)?;
                prev_writer.write_i64::<LittleEndian>(prev_header.next_page_id)?;
                prev_writer.write_u8(prev_header.flags)?;
                prev_writer.write_i32::<LittleEndian>(prev_header.data_length)?;
                prev_writer.write_all(&prev_header.padding)?;
                prev_writer.write_all(&prev_data)?;
                self.write_raw_page(prev_page_id, &prev_data, prev_header.version)?;
            }
            prev_page_id = current_page_id;
        }
        let index_root = self.document_index_root.lock();
        let document = Document {
            id,
            first_page_id,
            current_version: version,
            paths: vec![],
        };
        let data = self.serialize_document(&document)?;
        if index_root.page_id == -1 {
            let new_page_id = self.allocate_page()?;
            self.document_index_root.lock().page_id = new_page_id;
            self.document_index_root.lock().version = 1;
        }
        self.write_raw_page(index_root.page_id, &data, index_root.version)?;
        if let Some(old_version) = self.old_versions.lock().get(&id).cloned() {
            self.old_versions.lock().get_mut(&id).unwrap().push((version - 1, old_version[0].1));
        } else {
            self.old_versions.lock().insert(id, vec![(version - 1, first_page_id)]);
        }
        Ok(id)
    }

    fn read_document(&self, id: Uuid) -> Result<Vec<u8>, StreamDbError> {
        self.read_document_quick(id, self.quick_mode.load(Ordering::Relaxed))
    }

    fn read_document_quick(&self, id: Uuid, quick: bool) -> Result<Vec<u8>, StreamDbError> {
        let index_root = self.document_index_root.lock();
        let data = self.read_raw_page(index_root.page_id)?;
        let document = self.deserialize_document(&data)?;
        if document.id != id {
            return Err(StreamDbError::NotFound("Document not found".to_string()));
        }
        let mut result = Vec::new();
        let mut current_page_id = document.first_page_id;
        while current_page_id != -1 {
            let quick_mode = self.quick_mode.load(Ordering::Relaxed);
            self.quick_mode.store(quick, Ordering::Relaxed);
            let data = self.read_raw_page(current_page_id)?;
            self.quick_mode.store(quick_mode, Ordering::Relaxed);
            result.extend_from_slice(&data);
            let header = self.read_page_header(current_page_id)?;
            current_page_id = header.next_page_id;
        }
        Ok(result)
    }

    fn delete_document(&mut self, id: Uuid) -> Result<(), StreamDbError> {
        let index_root = self.document_index_root.lock();
        let data = self.read_raw_page(index_root.page_id)?;
        let document = self.deserialize_document(&data)?;
        if document.id != id {
            return Err(StreamDbError::NotFound("Document not found".to_string()));
        }
        let mut current_page_id = document.first_page_id;
        while current_page_id != -1 {
            let header = self.read_page_header(current_page_id)?;
            // Free page logic would go here
            current_page_id = header.next_page_id;
        }
        self.remove_from_index(id)?;
        Ok(())
    }

    fn bind_path_to_document(&mut self, path: &str, id: Uuid) -> Result<Uuid, StreamDbError> {
        if path.is_empty() || path.contains('\0') || path.contains("//") {
            return Err(StreamDbError::InvalidData("Invalid path".to_string()));
        }
        let index_root = self.document_index_root.lock();
        let data = self.read_raw_page(index_root.page_id)?;
        let mut document = self.deserialize_document(&data)?;
        if document.id != id {
            return Err(StreamDbError::NotFound("ID not found".to_string()));
        }
        if self.get_document_id_by_path(path).is_ok() {
            return Err(StreamDbError::InvalidData("Path already bound".to_string()));
        }
        document.paths.push(path.to_string());
        let data = self.serialize_document(&document)?;
        self.write_raw_page(index_root.page_id, &data, index_root.version)?;
        self.trie_insert(path, id)?;
        Ok(id)
    }

    fn get_document_id_by_path(&self, path: &str) -> Result<Uuid, StreamDbError> {
        if path.is_empty() || path.contains('\0') || path.contains("//") {
            return Err(StreamDbError::InvalidData("Invalid path".to_string()));
        }
        let trie_root = self.trie_root.lock();
        if trie_root.page_id == -1 {
            return Err(StreamDbError::NotFound("Path not found".to_string()));
        }
        let reversed: String = path.chars().rev().collect();
        let mut current_page_id = trie_root.page_id;
        let mut remaining = reversed.as_str();
        while !remaining.is_empty() {
            let node = self.deserialize_trie_node(&self.read_raw_page(current_page_id)?)?;
            let edge = node.edge.as_str();
            if remaining.starts_with(edge) {
                remaining = &remaining[edge.len()..];
                if remaining.is_empty() {
                    return node.document_id.ok_or_else(|| StreamDbError::NotFound("Path not found".to_string()));
                }
                let first_char = remaining.chars().next().unwrap();
                if let Some(&child_id) = node.children.get(&first_char) {
                    current_page_id = child_id;
                } else {
                    return Err(StreamDbError::NotFound("Path not found".to_string()));
                }
            } else {
                return Err(StreamDbError::NotFound("Path not found".to_string()));
            }
        }
        let node = self.deserialize_trie_node(&self.read_raw_page(current_page_id)?)?;
        node.document_id.ok_or_else(|| StreamDbError::NotFound("Path not found".to_string()))
    }

    fn search_paths(&self, prefix: &str) -> Result<Vec<String>, StreamDbError> {
        if prefix.is_empty() || prefix.contains('\0') || prefix.contains("//") {
            return Err(StreamDbError::InvalidData("Invalid prefix".to_string()));
        }
        let trie_root = self.trie_root.lock();
        if trie_root.page_id == -1 {
            return Ok(vec![]);
        }
        let reversed_prefix: String = prefix.chars().rev().collect();
        let mut current_page_id = trie_root.page_id;
        let mut remaining = reversed_prefix.as_str();
        while !remaining.is_empty() {
            let node = self.deserialize_trie_node(&self.read_raw_page(current_page_id)?)?;
            let edge = node.edge.as_str();
            if remaining.starts_with(edge) {
                remaining = &remaining[edge.len()..];
                if remaining.is_empty() {
                    let mut results = vec![];
                    self.trie_collect_paths(&node, String::new(), &mut results)?;
                    return Ok(results.into_iter().filter(|p| p.starts_with(prefix)).map(|p| p.chars().rev().collect()).collect());
                }
                let first_char = remaining.chars().next().unwrap();
                if let Some(&child_id) = node.children.get(&first_char) {
                    current_page_id = child_id;
                } else {
                    return Ok(vec![]);
                }
            } else {
                return Ok(vec![]);
            }
        }
        let node = self.deserialize_trie_node(&self.read_raw_page(current_page_id)?)?;
        let mut results = vec![];
        self.trie_collect_paths(&node, String::new(), &mut results)?;
        Ok(results.into_iter().filter(|p| p.starts_with(prefix)).map(|p| p.chars().rev().collect()).collect())
    }

    fn trie_collect_paths(&self, node: &ReverseTrieNode, prefix: String, results: &mut Vec<String>) -> Result<(), StreamDbError> {
        let new_prefix = format!("{}{}", node.edge, prefix);
        if let Some(id) = node.document_id {
            results.push(new_prefix.clone());
        }
        for &child_id in node.children.values() {
            let child = self.deserialize_trie_node(&self.read_raw_page(child_id)?)?;
            self.trie_collect_paths(&child, new_prefix.clone(), results)?;
        }
        Ok(())
    }

    fn list_paths_for_document(&self, id: Uuid) -> Result<Vec<String>, StreamDbError> {
        let index_root = self.document_index_root.lock();
        let data = self.read_raw_page(index_root.page_id)?;
        let document = self.deserialize_document(&data)?;
        if document.id != id {
            return Err(StreamDbError::NotFound("ID not found".to_string()));
        }
        Ok(document.paths)
    }

    fn count_free_pages(&self) -> Result<i64, StreamDbError> {
        let mut count = 0;
        let mut current = self.free_list_root.lock().page_id;
        while current != -1 {
            let offset = current as u64 * self.config.page_size + self.config.page_header_size;
            let mut buffer = vec![0u8; FREE_LIST_HEADER_SIZE as usize];
            {
                let mmap = self.mmap.read();
                if let Some(mmap) = mmap.as_ref() {
                    let start = offset as usize;
                    buffer.copy_from_slice(&mmap[start..start + FREE_LIST_HEADER_SIZE as usize]);
                } else {
                    let mut file = self.file.lock();
                    file.seek(SeekFrom::Start(offset))?;
                    file.read_exact(&mut buffer)?;
                }
            }
            let mut reader = Cursor::new(buffer);
            current = reader.read_i64::<LittleEndian>()?;
            let used_entries = reader.read_i32::<LittleEndian>()?;
            count += used_entries as i64;
        }
        Ok(count)
    }

    fn get_info(&self, id: Uuid) -> Result<String, StreamDbError> {
        let index_root = self.document_index_root.lock();
        let data = self.read_raw_page(index_root.page_id)?;
        let document = self.deserialize_document(&data)?;
        if document.id != id {
            return Err(StreamDbError::NotFound("ID not found".to_string()));
        }
        let mut size = 0;
        let mut current_page_id = document.first_page_id;
        while current_page_id != -1 {
            let header = self.read_page_header(current_page_id)?;
            size += header.data_length as u64;
            current_page_id = header.next_page_id;
        }
        Ok(format!(
            "ID: {}, Version: {}, Size: {} bytes, Paths: {:?}",
            id, document.current_version, size, document.paths
        ))
    }

    fn delete_paths_for_document(&mut self, id: Uuid) -> Result<(), StreamDbError> {
        let index_root = self.document_index_root.lock();
        let data = self.read_raw_page(index_root.page_id)?;
        let mut document = self.deserialize_document(&data)?;
        if document.id != id {
            return Err(StreamDbError::NotFound("ID not found".to_string()));
        }
        for path in document.paths.iter() {
            // Implement trie_delete
        }
        document.paths.clear();
        let data = self.serialize_document(&document)?;
        self.write_raw_page(index_root.page_id, &data, index_root.version)?;
        Ok(())
    }

    fn remove_from_index(&mut self, id: Uuid) -> Result<(), StreamDbError> {
        self.delete_paths_for_document(id)?;
        Ok(())
    }

    fn get_cache_stats(&self) -> Result<CacheStats, StreamDbError> {
        Ok(self.cache_stats.lock().clone())
    }

    fn get_stream(&self, id: Uuid) -> Result<impl Iterator<Item = Result<Vec<u8>, StreamDbError>>, StreamDbError> {
        let index_root = self.document_index_root.lock();
        let data = self.read_raw_page(index_root.page_id)?;
        let document = self.deserialize_document(&data)?;
        if document.id != id {
            return Err(StreamDbError::NotFound("Document not found".to_string()));
        }
        let mut current = Some(document.first_page_id);
        Ok(std::iter::from_fn(move || {
            if let Some(page_id) = current {
                let data = self.read_raw_page(page_id);
                if let Ok(ref d) = data {
                    let header = self.read_page_header(page_id).ok()?;
                    current = if header.next_page_id != -1 { Some(header.next_page_id) } else { None };
                }
                data.map(Ok)
            } else {
                None
            }
        }))
    }
}

pub struct StreamDb {
    backend: Box<dyn DatabaseBackend + Send + Sync>,
    path_cache: Mutex<LruCache<String, Uuid>>,
    quick_mode: AtomicBool,
}

impl StreamDb {
    pub fn open_with_config<P: AsRef<Path>>(path: P, config: Config) -> Result<Self, StreamDbError> {
        let backend = Box::new(FileBackend::new(path, config)?);
        Ok(Self {
            backend,
            path_cache: Mutex::new(LruCache::new(config.page_cache_size)),
            quick_mode: AtomicBool::new(false),
        })
    }
}

impl Database for StreamDb {
    fn write_document(&mut self, path: &str, data: &mut dyn Read) -> Result<Uuid, StreamDbError> {
        let id = self.backend.write_document(data)?;
        self.backend.bind_path_to_document(path, id)?;
        self.path_cache.lock().put(path.to_string(), id);
        Ok(id)
    }

    fn get(&self, path: &str) -> Result<Vec<u8>, StreamDbError> {
        self.get_quick(path, self.quick_mode.load(Ordering::Relaxed))
    }

    fn get_quick(&self, path: &str, quick: bool) -> Result<Vec<u8>, StreamDbError> {
        let id = self.get_id_by_path(path)?;
        if let Some(id) = id {
            self.backend.read_document_quick(id, quick)
        } else {
            Err(StreamDbError::NotFound("Path not found".to_string()))
        }
    }

    fn get_id_by_path(&self, path: &str) -> Result<Option<Uuid>, StreamDbError> {
        if let Some(id) = self.path_cache.lock().get(path) {
            return Ok(Some(*id));
        }
        let id = self.backend.get_document_id_by_path(path);
        if let Ok(id) = id {
            self.path_cache.lock().put(path.to_string(), id);
            Ok(Some(id))
        } else {
            Ok(None)
        }
    }

    fn delete(&mut self, path: &str) -> Result<(), StreamDbError> {
        if let Some(id) = self.get_id_by_path(path)? {
            self.backend.delete_document(id)?;
            self.path_cache.lock().pop(path);
        }
        Ok(())
    }

    fn delete_by_id(&mut self, id: Uuid) -> Result<(), StreamDbError> {
        self.backend.delete_document(id)?;
        self.path_cache.lock().retain(|_, v| *v != id);
        Ok(())
    }

    fn bind_to_path(&mut self, id: Uuid, path: &str) -> Result<(), StreamDbError> {
        self.backend.bind_path_to_document(path, id)?;
        self.path_cache.lock().put(path.to_string(), id);
        Ok(())
    }

    fn unbind_path(&mut self, id: Uuid, path: &str) -> Result<(), StreamDbError> {
        self.backend.delete_paths_for_document(id)?;
        self.path_cache.lock().pop(path);
        Ok(())
    }

    fn search(&self, prefix: &str) -> Result<Vec<String>, StreamDbError> {
        self.backend.search_paths(prefix)
    }

    fn list_paths(&self, id: Uuid) -> Result<Vec<String>, StreamDbError> {
        self.backend.list_paths_for_document(id)
    }

    fn flush(&self) -> Result<(), StreamDbError> {
        // Implement flush for FileBackend
        Ok(())
    }

    fn calculate_statistics(&self) -> Result<(i64, i64), StreamDbError> {
        let free_pages = self.backend.count_free_pages()?;
        Ok((free_pages, 0)) // Total pages not implemented
    }

    fn set_quick_mode(&mut self, enabled: bool) {
        self.quick_mode.store(enabled, Ordering::Relaxed);
    }

    fn snapshot(&self) -> Result<Self, StreamDbError> {
        // Implement snapshot
        Err(StreamDbError::NotFound("Snapshot not implemented".to_string()))
    }

    fn get_cache_stats(&self) -> Result<CacheStats, StreamDbError> {
        self.backend.get_cache_stats()
    }

    fn get_stream(&self, path: &str) -> Result<impl Iterator<Item = Result<Vec<u8>, StreamDbError>>, StreamDbError> {
        let id = self.get_id_by_path(path)?;
        if let Some(id) = id {
            self.backend.get_stream(id)
        } else {
            Err(StreamDbError::NotFound("Path not found".to_string()))
        }
    }

    fn get_async(&self, path: &str) -> impl Future<Output = Result<Vec<u8>, StreamDbError>> {
        async move { self.get(path) }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
enum DCFEvent {
    Send { msg_id: String, timestamp: i64, content_summary: String },
    Receive { msg_id: String, timestamp: i64, content_summary: String },
    Failure { error: String, timestamp: i64, context: String },
}

#[derive(Debug, Deserialize, Clone)]
pub struct DcfConfig {
    pub streamdb: Config,
    pub grpc_port: u16,
    pub p2p_discovery: bool,
}

impl Default for DcfConfig {
    fn default() -> Self {
        Self {
            streamdb: Config::default(),
            grpc_port: 50051,
            p2p_discovery: true,
        }
    }
}

pub struct MyDcfService {
    db: Arc<StreamDb>,
    role: Mutex<String>,
    peers: Mutex<HashMap<String, i64>>,
    metrics: Mutex<Metrics>,
    master_address: Mutex<String>,
}

impl MyDcfService {
    pub fn new(db: Arc<StreamDb>) -> Self {
        Self {
            db,
            role: Mutex::new("p2p".to_string()),
            peers: Mutex::new(HashMap::new()),
            metrics: Mutex::new(Metrics { sends: 0, receives: 0, failures: 0 }),
            master_address: Mutex::new("".to_string()),
        }
    }

    async fn log_event(&self, event: DCFEvent) -> Result<(), StreamDbError> {
        let timestamp = SystemTime::now().duration_since(UNIX_EPOCH)?.as_secs() as i64;
        let event_id = Uuid::new_v4();
        let path = format!("/logs/{}-{}.json", timestamp, event_id);
        let json = serde_json::to_vec(&event)?;
        let mut data = Cursor::new(json);
        self.db.write_document(&path, &mut data)?;
        self.db.flush()?;
        Ok(())
    }

    async fn discover_peers(&self, config: &DcfConfig) -> Result<(), StreamDbError> {
        if !config.p2p_discovery {
            return Ok(());
        }
        let mdns = ServiceDaemon::new()?;
        let service_type = "_dcf._tcp.local.";
        let receiver = mdns.browse(service_type)?;
        while let Ok(event) = receiver.recv_async().await {
            if let mdns_sd::ServiceEvent::ServiceResolved(info) = event {
                let addr = info.get_addresses().iter().next().map(|a| a.to_string()).unwrap_or_default();
                let rtt = measure_rtt(&addr).await.unwrap_or(100);
                if rtt < 50 {
                    self.peers.lock().insert(addr.clone(), rtt);
                    let path = format!("/peers/{}", Uuid::new_v4());
                    let peer_info = PeerInfo { address: addr.clone(), rtt };
                    let mut data = Cursor::new(serde_json::to_vec(&peer_info)?);
                    self.db.write_document(&path, &mut data)?;
                    self.db.flush()?;
                    let event = DCFEvent::Receive {
                        msg_id: addr,
                        timestamp: SystemTime::now().duration_since(UNIX_EPOCH)?.as_secs() as i64,
                        content_summary: format!("RTT: {}", rtt),
                    };
                    self.log_event(event).await?;
                }
            }
        }
        Ok(())
    }
}

async fn measure_rtt(address: &str) -> Result<i64, StreamDbError> {
    // Placeholder: Real implementation would use TCP or ICMP ping
    Ok(rand::random::<i64>() % 100)
}
