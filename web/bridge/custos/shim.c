/* SPDX-License-Identifier: LGPL-3.0-only
 * Copyright (c) 2026 DeMoD LLC.
 *
 * web/bridge/custos/shim.c -- the one symbol custos.gen.c imports.
 *
 * custos.gen.c is an Exsecutor library unit (exsc --emitte c): no entry point,
 * no runtime, and exactly one import, exsrt_abortus, which a bounds or
 * overflow trap calls. exs_admitte reads at most 32 bytes and traps on nothing
 * the Rust wrapper (src/gate.rs) can pass, so reaching this is a defect in the
 * gate or its build. A gate that has failed must not keep admitting, and the
 * prototype is _Noreturn, so the process stops: fail closed. */
#include <stdio.h>
#include <stdlib.h>

_Noreturn void exsrt_abortus(unsigned kind);

_Noreturn void exsrt_abortus(unsigned kind)
{
  fprintf(stderr, "dcf-ws-bridge: custos gate trapped (exsrt_abortus %u); stopping\n", kind);
  abort();
}
