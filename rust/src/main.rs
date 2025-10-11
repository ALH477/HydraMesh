use dcf_rust_sdk::{DcfConfig, MyDcfService, StreamDb};
use std::sync::Arc;
use tonic::{transport::Server, Request, Response, Status};
use dcf_rust_sdk::proto::dcf::dcf_service_server::{DcfService, DcfServiceServer};
use dcf_rust_sdk::proto::dcf::{DcfMessage, DcfResponse, Metrics, RoleAssignment, PeerInfo, Empty};

mod proto {
    tonic::include_proto!("dcf");
}

#[async_trait::async_trait]
impl DcfService for MyDcfService {
    async fn send_message(&self, request: Request<DcfMessage>) -> Result<Response<DcfResponse>, Status> {
        let msg = request.into_inner();
        let path = format!("/messages/{}", msg.id);
        let mut data = Cursor::new(serde_json::to_vec(&msg).map_err(|e| Status::internal(e.to_string()))?);
        let id = self.db.write_document(&path, &mut data).map_err(|e| Status::internal(e.to_string()))?;
        self.db.flush().map_err(|e| Status::internal(e.to_string()))?;
        let mut metrics = self.metrics.lock();
        metrics.sends += 1;
        let metrics_path = "/metrics";
        let mut metrics_data = Cursor::new(serde_json::to_vec(&*metrics).map_err(|e| Status::internal(e.to_string()))?);
        self.db.write_document(metrics_path, &mut metrics_data).map_err(|e| Status::internal(e.to_string()))?;
        self.db.flush().map_err(|e| Status::internal(e.to_string()))?;
        let event = DCFEvent::Send {
            msg_id: msg.id.clone(),
            timestamp: msg.timestamp,
            content_summary: msg.content.chars().take(50).collect(),
        };
        self.log_event(event).await.map_err(|e| Status::internal(e.to_string()))?;
        Ok(Response::new(DcfResponse { success: true, message: "Received".to_string() }))
    }

    async fn get_metrics(&self, _request: Request<Empty>) -> Result<Response<Metrics>, Status> {
        Ok(Response::new(self.metrics.lock().clone()))
    }

    async fn assign_role(&self, request: Request<RoleAssignment>) -> Result<Response<DcfResponse>, Status> {
        let assign = request.into_inner();
        *self.role.lock() = assign.role.clone();
        *self.master_address.lock() = assign.master_address.clone();
        let role_path = "/role";
        let mut role_data = Cursor::new(serde_json::to_vec(&assign).map_err(|e| Status::internal(e.to_string()))?);
        if let Err(e) = self.db.write_document(role_path, &mut role_data) {
            let event = DCFEvent::Failure {
                error: e.to_string(),
                timestamp: SystemTime::now().duration_since(UNIX_EPOCH).map(|d| d.as_secs() as i64).unwrap_or(0),
                context: "Role assignment write failed".to_string(),
            };
            self.log_event(event).await.ok();
            return Err(Status::internal(e.to_string()));
        }
        self.db.flush().map_err(|e| Status::internal(e.to_string()))?;
        Ok(Response::new(DcfResponse { success: true, message: "Role assigned".to_string() }))
    }

    async fn report_peer(&self, request: Request<PeerInfo>) -> Result<Response<DcfResponse>, Status> {
        let peer = request.into_inner();
        self.peers.lock().insert(peer.address.clone(), peer.rtt);
        let path = format!("/peers/{}", Uuid::new_v4());
        let mut data = Cursor::new(serde_json::to_vec(&peer).map_err(|e| Status::internal(e.to_string()))?);
        self.db.write_document(&path, &mut data).map_err(|e| Status::internal(e.to_string()))?;
        self.db.flush().map_err(|e| Status::internal(e.to_string()))?;
        let event = DCFEvent::Receive {
            msg_id: peer.address.clone(),
            timestamp: SystemTime::now().duration_since(UNIX_EPOCH).map(|d| d.as_secs() as i64).unwrap_or(0),
            content_summary: format!("RTT: {}", peer.rtt),
        };
        self.log_event(event).await.map_err(|e| Status::internal(e.to_string()))?;
        Ok(Response::new(DcfResponse { success: true, message: "Peer reported".to_string() }))
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::init();
    let mut settings = config::Config::builder()
        .add_source(config::File::with_name("dcf_config.toml").required(false))
        .add_source(config::Environment::with_prefix("DCF").separator("_"))
        .build()?;
    let dcf_config: DcfConfig = settings.try_deserialize()?;
    let db = Arc::new(StreamDb::open_with_config("dcf.db", dcf_config.streamdb)?);
    let service = MyDcfService::new(db.clone());
    let addr = format!("[::1]:{}", dcf_config.grpc_port).parse()?;
    // Start P2P discovery in the background
    let discovery_config = dcf_config.clone();
    let discovery_service = service.clone();
    tokio::spawn(async move {
        if let Err(e) = discovery_service.discover_peers(&discovery_config).await {
            log::error!("Peer discovery failed: {}", e);
        }
    });
    Server::builder()
        .add_service(DcfServiceServer::new(service))
        .serve(addr)
        .await?;
    Ok(())
}
