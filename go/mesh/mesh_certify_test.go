// SPDX-License-Identifier: LGPL-3.0-only

package mesh

import (
	"encoding/json"
	"os"
	"reflect"
	"testing"
)

type meshVectors struct {
	FSM []struct {
		Events        []byte `json:"events"`
		FailThreshold int    `json:"fail_threshold"`
		OkThreshold   int    `json:"ok_threshold"`
		Status        int    `json:"status"`
	} `json:"fsm"`
	Grouping []struct {
		RTT       int `json:"rtt"`
		Threshold int `json:"threshold"`
		Status    int `json:"status"`
		Group     int `json:"group"`
	} `json:"grouping"`
	Dijkstra []struct {
		N       int      `json:"n"`
		Edges   [][3]int `json:"edges"`
		Source  int      `json:"source"`
		Dist    []int    `json:"dist"`
		NextHop []int    `json:"next_hop"`
	} `json:"dijkstra"`
	Routes []struct {
		Candidates [][3]int `json:"candidates"`
		Ordered    []int    `json:"ordered"`
	} `json:"routes"`
	Election []struct {
		N             int      `json:"n"`
		Edges         [][3]int `json:"edges"`
		RelayMinDeg   int      `json:"relay_min_degree"`
		Master        int      `json:"master"`
		Roles         []int    `json:"roles"`
	} `json:"election"`
}

func loadMesh(t *testing.T) meshVectors {
	t.Helper()
	for _, p := range []string{
		"../../Documentation/mesh_vectors.json",
		"../../python/MCP/mesh_vectors.json",
	} {
		data, err := os.ReadFile(p)
		if err != nil {
			continue
		}
		var v meshVectors
		if err := json.Unmarshal(data, &v); err != nil {
			t.Fatalf("parse %s: %v", p, err)
		}
		return v
	}
	t.Fatal("mesh_vectors.json not found (run gen_mesh_vectors.py)")
	return meshVectors{}
}

func TestMeshAlgorithmsMatchGolden(t *testing.T) {
	v := loadMesh(t)
	for i, c := range v.FSM {
		if got := PeerStatus(c.Events, c.FailThreshold, c.OkThreshold); got != c.Status {
			t.Errorf("fsm[%d]: got %d want %d", i, got, c.Status)
		}
	}
	for i, c := range v.Grouping {
		if got := GroupOf(c.RTT, c.Threshold, c.Status); got != c.Group {
			t.Errorf("grouping[%d]: got %d want %d", i, got, c.Group)
		}
	}
	for i, c := range v.Dijkstra {
		dist, nh := Dijkstra(c.N, c.Edges, c.Source)
		if !reflect.DeepEqual(dist, c.Dist) {
			t.Errorf("dijkstra[%d] dist: got %v want %v", i, dist, c.Dist)
		}
		if !reflect.DeepEqual(nh, c.NextHop) {
			t.Errorf("dijkstra[%d] next_hop: got %v want %v", i, nh, c.NextHop)
		}
	}
	for i, c := range v.Routes {
		got := SelectRoutes(c.Candidates)
		if len(got) == 0 && len(c.Ordered) == 0 {
			continue
		}
		if !reflect.DeepEqual(got, c.Ordered) {
			t.Errorf("routes[%d]: got %v want %v", i, got, c.Ordered)
		}
	}
	for i, c := range v.Election {
		m, roles := Elect(c.N, c.Edges, c.RelayMinDeg)
		if m != c.Master || !reflect.DeepEqual(roles, c.Roles) {
			t.Errorf("election[%d]: master %d/%d roles %v/%v", i, m, c.Master, roles, c.Roles)
		}
	}
	t.Logf("mesh: %d fsm, %d grouping, %d dijkstra, %d routes, %d election",
		len(v.FSM), len(v.Grouping), len(v.Dijkstra), len(v.Routes), len(v.Election))
}
