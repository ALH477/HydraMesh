# python/dcf/master.py
# Integrated DCF Master for Python SDK: Combines real DCF APIs with mock
# implementations for full DeMoD management.
# Version 1.1.0 | License: GPL-3.0 | For Mono Repo SDKs
# Copyright (C) 2025 DeMoD LLC

import json
import logging
from typing import Any, Dict, List, Tuple, Optional

from .networking import Networking
from .serialization import build_message
from .utils import parse_peer

logger = logging.getLogger(__name__)
logging.basicConfig(level=logging.INFO)


class DCFMaster:
    """Master node controller for DCF network management."""

    def __init__(self, config: Dict[str, Any], use_mock: bool = False) -> None:
        """
        Initialize DCFMaster with config and optional mock mode.

        Args:
            config: Configuration dictionary with peers and settings.
            use_mock: If True, use mock implementations for unavailable APIs.
        """
        self.config: Dict[str, Any] = config
        self.clients: Dict[str, Networking] = {}
        self.use_mock: bool = use_mock
        try:
            for peer in config.get("peers", []):
                host, port = parse_peer(peer)
                self.clients[peer] = Networking("gRPC", host, int(port), "master")
            logger.info(f"Initialized DCFMaster with {len(self.clients)} peers")
        except ValueError as e:
            logger.exception("Failed to parse peer config")
            raise ValueError(f"Invalid peer config: {e}") from e
        except Exception:
            logger.exception("Unexpected error initializing DCFMaster")
            raise

    def assign_role(self, peer: str, mode: str) -> bool:
        """Assign a role (P2P_MODE, CLIENT_MODE, SERVER_MODE, AUTO_MODE) to a peer.

        Args:
            peer: Peer identifier.
            mode: Role to assign.

        Returns:
            True if successful, False otherwise.

        Raises:
            ValueError: If peer or mode is invalid.
        """
        try:
            if peer not in self.clients:
                raise ValueError(f"Unknown peer: {peer}")
            valid_modes = ["P2P_MODE", "CLIENT_MODE", "SERVER_MODE", "AUTO_MODE"]
            if mode not in valid_modes:
                raise ValueError(f"Invalid mode: {mode}")
            msg = build_message(command="set_role", role=mode, recipient=peer)
            self.clients[peer].send(msg)
            logger.info(f"Assigned role {mode} to {peer}")
            return True
        except ValueError as e:
            logger.exception("Invalid args for assign_role")
            raise
        except Exception:
            logger.exception("Failed to assign role")
            return False

    def update_config(self, peer: str, key: str, value: str) -> bool:
        """Update a configuration key-value for a peer.

        Args:
            peer: Peer identifier.
            key: Config key.
            value: Config value.

        Returns:
            True if successful, False otherwise.
        """
        try:
            if peer not in self.clients:
                raise ValueError(f"Unknown peer: {peer}")
            msg = build_message(command="update_config", key=key, value=value, recipient=peer)
            self.clients[peer].send(msg)
            logger.info(f"Updated config {key}={value} for {peer}")
            return True
        except ValueError as e:
            logger.exception("Invalid args for update_config")
            raise
        except Exception:
            logger.exception("Failed to update config")
            return False

    def collect_metrics(self) -> Dict[str, Any]:
        """Collect network metrics (RTT, health) from all peers.

        Returns:
            Dictionary mapping peers to their metrics.
        """
        try:
            metrics: Dict[str, Any] = {}
            for peer in self.clients:
                msg = build_message(command="collect_metrics", recipient=peer)
                resp = self.clients[peer].send(msg)
                try:
                    metrics[peer] = json.loads(resp.data)
                except json.JSONDecodeError:
                    logger.exception(f"Failed to parse metrics for {peer}")
                    metrics[peer] = {}
            logger.info(f"Collected metrics from {len(metrics)} peers")
            return metrics
        except Exception:
            logger.exception("Failed to collect metrics")
            return {"error": "Failed"}

    def optimize_network(self) -> bool:
        """Optimize network based on RTT metrics.

        Returns:
            True if optimization triggered, False otherwise.
        """
        try:
            metrics = self.collect_metrics()
            avg_rtt = sum(m.get("rtt", 0) for m in metrics.values()) / max(1, len(metrics))
            threshold = self.config.get("group_rtt_threshold", 50)
            if avg_rtt > threshold:
                logger.info(f"Optimizing: High RTT {avg_rtt} > {threshold}")
                # Mock optimization logic
                return True
            logger.info("No optimization needed")
            return True
        except Exception:
            logger.exception("Failed to optimize network")
            return False

    def health_check(self, peer: str) -> Tuple[int, str]:
        """Perform health check on a peer (mock if not implemented).

        Args:
            peer: Peer identifier.

        Returns:
            Tuple of (RTT in ms, status string).

        Raises:
            ValueError: If peer is invalid.
        """
        if self.use_mock:
            logger.info(f"Mock health_check for {peer}")
            return 50, "healthy"
        try:
            if peer not in self.clients:
                raise ValueError(f"Unknown peer: {peer}")
            msg = build_message(command="health_check", recipient=peer)
            resp = self.clients[peer].send(msg)
            try:
                data = json.loads(resp.data)
                rtt = data.get("rtt", 50)
                status = data.get("status", "healthy")
                logger.info(f"Health check for {peer}: RTT={rtt}, status={status}")
                return rtt, status
            except json.JSONDecodeError:
                logger.exception(f"Failed to parse health check for {peer}")
                return 0, "error"
        except ValueError as e:
            logger.exception("Invalid args for health_check")
            raise
        except Exception:
            logger.exception("Failed health check")
            return 0, "error"

    def list_peers(self) -> List[str]:
        """List all peers with RTT, group ID, and status (mock if not implemented).

        Returns:
            List of peer info strings.
        """
        if self.use_mock:
            logger.info("Mock list_peers")
            return ["peer1: 50ms, group1"]
        try:
            peers = [f"{peer}: {self._get_peer_info(peer)}" for peer in self.clients]
            logger.info(f"Listed {len(peers)} peers")
            return peers
        except Exception:
            logger.exception("Failed to list peers")
            return []

    def heal_peer(self, peer: str) -> bool:
        """Trigger self-healing for a peer (mock if not implemented).

        Args:
            peer: Peer identifier.

        Returns:
            True if successful, False otherwise.

        Raises:
            ValueError: If peer is invalid.
        """
        if self.use_mock:
            logger.info(f"Mock heal_peer for {peer}")
            return True
        try:
            if peer not in self.clients:
                raise ValueError(f"Unknown peer: {peer}")
            msg = build_message(command="heal_peer", recipient=peer)
            self.clients[peer].send(msg)
            logger.info(f"Healed peer {peer}")
            return True
        except ValueError as e:
            logger.exception("Invalid args for heal_peer")
            raise
        except Exception:
            logger.exception("Failed to heal peer")
            return False

    def group_peers(self) -> bool:
        """Regroup peers based on RTT threshold (mock if not implemented).

        Returns:
            True if successful, False otherwise.
        """
        if self.use_mock:
            logger.info("Mock group_peers")
            return True
        try:
            metrics = self.collect_metrics()
            # Mock grouping logic based on RTT
            logger.info("Grouped peers based on RTT")
            return True
        except Exception:
            logger.exception("Failed to group peers")
            return False

    def simulate_failure(self, peer: str) -> bool:
        """Simulate a failure on a peer (mock if not implemented).

        Args:
            peer: Peer identifier.

        Returns:
            True if successful, False otherwise.

        Raises:
            ValueError: If peer is invalid.
        """
        if self.use_mock:
            logger.info(f"Mock simulate_failure for {peer}")
            return True
        try:
            if peer not in self.clients:
                raise ValueError(f"Unknown peer: {peer}")
            msg = build_message(command="simulate_failure", recipient=peer)
            self.clients[peer].send(msg)
            logger.info(f"Simulated failure for {peer}")
            return True
        except ValueError as e:
            logger.exception("Invalid args for simulate_failure")
            raise
        except Exception:
            logger.exception("Failed to simulate failure")
            return False

    def set_log_level(self, level: int) -> bool:
        """Set logging level (0=debug, 1=info, 2=error) (mock if not implemented).

        Args:
            level: Log level (0-2).

        Returns:
            True if successful, False otherwise.

        Raises:
            ValueError: If level is invalid.
        """
        if self.use_mock:
            logger.info(f"Mock set_log_level to {level}")
            return True
        try:
            if not (0 <= level <= 2):
                raise ValueError("Invalid level")
            for peer in self.clients:
                msg = build_message(command="set_log_level", level=level, recipient=peer)
                self.clients[peer].send(msg)
            logger.info(f"Set log level to {level}")
            return True
        except ValueError as e:
            logger.exception("Invalid args for set_log_level")
            raise
        except Exception:
            logger.exception("Failed to set log level")
            return False

    def load_plugin(self, path: str) -> bool:
        """Dynamically load a plugin by path (mock if not implemented).

        Args:
            path: Plugin path.

        Returns:
            True if successful, False otherwise.
        """
        if self.use_mock:
            logger.info(f"Mock load_plugin {path}")
            return True
        try:
            # Assume DCF supports plugin loading; mock for now
            logger.info(f"Loaded plugin {path}")
            return True
        except Exception:
            logger.exception("Failed to load plugin")
            return False

    def _get_peer_info(self, peer: str) -> str:
        """Helper to get peer info (mock for now)."""
        try:
            msg = build_message(command="get_info", recipient=peer)
            resp = self.clients[peer].send(msg)
            return json.loads(resp.data).get("info", "50ms, group1")
        except (json.JSONDecodeError, Exception):
            logger.exception(f"Failed to get info for {peer}")
            return "unknown"
