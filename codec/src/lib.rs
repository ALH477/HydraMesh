#[path = "../frame.rs"]
mod frame;

pub use frame::{crc16_ccitt, Frame, FrameError, FrameType, BROADCAST, FRAME_SIZE, SYNC_BYTE};