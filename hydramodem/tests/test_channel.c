/* tests/test_channel.c -- modulation orders and realistic channel impairments.
 *
 * The end-to-end loopback proves the binary-FSK link over AWGN; this proves the
 * claims that the loopback does not:
 *   [1] M-FSK   -- 4/8/16-ary modulation actually decodes (incl. the zero-pad
 *                  path when bits/symbol does not divide the sync or coded bits).
 *   [2] bursts  -- the interleaver earns its place: under a long error burst,
 *                  conv+interleave vastly outperforms conv-only (a convolutional
 *                  code alone cannot ride out a long consecutive error run).
 *   [3] reverb  -- multipath/ISI behaviour, and that lowering the baud (longer
 *                  symbols) restores margin on a reverberant channel.
 *
 * These are characterization tests with deliberately wide pass margins so they
 * are regressions, not flaky thresholds.
 */
#include "../src/hydra_modem.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static int g_fail = 0;
#define CHECK(cond, msg) do { \
    if (cond) printf("  ok   %s\n", msg); \
    else    { printf("  FAIL %s\n", msg); ++g_fail; } } while (0)

static uint64_t rng = 0xDEADBEEFULL;
static double u(void){ rng^=rng<<13; rng^=rng>>7; rng^=rng<<17; return (double)(rng>>11)/9007199254740992.0; }
static double gs(void){ double a=u(),b=u(); if(a<1e-12)a=1e-12; return sqrt(-2*log(a))*cos(2*M_PI*b); }
static double sigp(const float*x,size_t n){ double a=0; size_t i,c=0;
    for(i=0;i<n;i++){ if(fabsf(x[i])>1e-6f){ a+=(double)x[i]*x[i]; ++c; } }
    return c?a/c:0; }
static void awgn(float*x,size_t n,double snr){ double sd=sqrt(sigp(x,n)/pow(10,snr/10));
    size_t i; for(i=0;i<n;i++) x[i]+=(float)(sd*gs()); }

/* ----------------------------------------------------------------- M-FSK ---- */
static int mfsk_rate(int ntones,double snr)
{
    hydra_profile p; hydra_profile_default(&p);
    p.n_tones = ntones; p.base_freq = 2000; p.tone_spacing = 1000;
    if (hydra_profile_init(&p) != 0) return -1;
    int ok=0,TR=50,t,i;
    for(t=0;t<TR;t++){
        uint8_t tx[17],out[17]; for(i=0;i<17;i++) tx[i]=(uint8_t)(u()*256);
        float*a=NULL; size_t n=0; if(hydra_modem_tx(&p,tx,&a,&n)) continue;
        if(snr<99) awgn(a,n,snr);
        if(hydra_modem_rx(&p,a,n,out)==HYDRA_OK && !memcmp(tx,out,17)) ++ok;
        free(a);
    }
    return ok*100/TR;
}

/* ----------------------------------------------------------- burst channel -- */
static int burst_rate(int interleave,int burst_syms)
{
    hydra_profile p; hydra_profile_default(&p);
    p.fec_mode=HYDRA_FEC_CONV; p.interleave=interleave; hydra_profile_init(&p);
    int spp=p.samples_per_symbol, TR=80, ok=0, t;
    size_t lead=(size_t)(0.02*p.sample_rate);
    size_t data_start=lead+((size_t)p.preamble_syms+p.sync_syms)*(size_t)spp;
    for(t=0;t<TR;t++){
        uint8_t tx[17],out[17]; int i; for(i=0;i<17;i++) tx[i]=(uint8_t)(u()*256);
        float*a=NULL; size_t n=0; if(hydra_modem_tx(&p,tx,&a,&n)) continue;
        double sd=sqrt(sigp(a,n)/pow(10,12.0/10));
        { size_t k; for(k=0;k<n;k++) a[k]+=(float)(sd*gs()); }
        size_t flen=(size_t)burst_syms*(size_t)spp;
        size_t span=(n>data_start+flen)?(n-data_start-flen):0;
        size_t st=data_start+(size_t)(u()*(double)span), k;
        for(k=0;k<flen && st+k<n;k++) a[st+k]=(float)(sd*gs());     /* deep fade */
        if(hydra_modem_rx(&p,a,n,out)==HYDRA_OK && !memcmp(tx,out,17)) ++ok;
        free(a);
    }
    return ok*100/TR;
}

/* ---------------------------------------------------------- reverb channel -- */
static float* reverb(const float*x,size_t n,double rt_ms,double sr,size_t*outn)
{
    size_t tail=(size_t)(rt_ms*sr/1000.0); if(tail<1)tail=1;
    size_t m=n+tail,i,d; float*h=calloc(tail+1,sizeof*h); float*y=calloc(m,sizeof*y);
    double tau=(double)tail/6.9;
    h[0]=1.0f;
    for(d=1;d<=tail;d++) h[d]=(float)(exp(-(double)d/tau)*(2.0*u()-1.0)*0.5);
    for(i=0;i<n;i++){ if(x[i]==0.0f) continue; for(d=0;d<=tail;d++) y[i+d]+=x[i]*h[d]; }
    { double pin=sigp(x,n),pout=sigp(y,m),g=(pout>0)?sqrt(pin/pout):1.0;
      for(i=0;i<m;i++) y[i]=(float)(y[i]*g); }
    free(h); *outn=m; return y;
}
static int reverb_rate(double baud,double rt_ms)
{
    hydra_profile p; hydra_profile_default(&p); p.baud=baud;
    p.base_freq=(baud<=250)?1000:2000; p.tone_spacing=(baud<=250)?500:1000;
    if(hydra_profile_init(&p)) return -1;
    int ok=0,TR=60,t;
    for(t=0;t<TR;t++){
        uint8_t tx[17],out[17]; int i; for(i=0;i<17;i++) tx[i]=(uint8_t)(u()*256);
        float*a=NULL; size_t n=0; if(hydra_modem_tx(&p,tx,&a,&n)) continue;
        size_t rn; float*ry=reverb(a,n,rt_ms,p.sample_rate,&rn);
        if(hydra_modem_rx(&p,ry,rn,out)==HYDRA_OK && !memcmp(tx,out,17)) ++ok;
        free(a); free(ry);
    }
    return ok*100/TR;
}

int main(void)
{
    printf("HydraModem channel / modulation tests\n\n");

    printf("[1] M-FSK orders (clean | +0 dB AWGN)\n");
    { int r2c=mfsk_rate(2,99), r4c=mfsk_rate(4,99), r8c=mfsk_rate(8,99), r16c=mfsk_rate(16,99);
      int r2n=mfsk_rate(2,0),  r4n=mfsk_rate(4,0),  r8n=mfsk_rate(8,0);
      printf("    2-FSK %d%%|%d%%  4-FSK %d%%|%d%%  8-FSK %d%%|%d%%  16-FSK %d%%|-\n",
             r2c,r2n,r4c,r4n,r8c,r8n,r16c);
      CHECK(r2c==100&&r4c==100&&r8c==100&&r16c==100, "2/4/8/16-FSK clean loopback all 100%");
      CHECK(r2n>=95&&r4n>=95&&r8n>=80, "2/4/8-FSK robust at 0 dB AWGN");
    }

    printf("\n[2] burst-error channel (CONV, 12 dB AWGN + deep fade)\n");
    { int b16_off=burst_rate(0,16), b16_on=burst_rate(1,16);
      int b40_off=burst_rate(0,40), b40_on=burst_rate(1,40);
      printf("    16-sym burst: conv-only %d%%  conv+interleave %d%%\n", b16_off,b16_on);
      printf("    40-sym burst: conv-only %d%%  conv+interleave %d%%\n", b40_off,b40_on);
      CHECK(b40_on>=90, "conv+interleave rides out a 40-symbol burst (>=90%)");
      CHECK(b40_off<=40, "conv-only fails the 40-symbol burst (<=40%)");
      CHECK(b16_on>=b16_off, "interleave never worse than conv-only on bursts");
    }

    printf("\n[3] reverb / multipath (CONV+interleave)\n");
    { int r0=reverb_rate(1000,0);
      int hi50=reverb_rate(1000,50), lo50=reverb_rate(125,50);
      int lo20=reverb_rate(125,20);
      printf("    clean RT60=0: %d%%\n", r0);
      printf("    RT60=50ms: 1000 baud %d%%  ->  125 baud %d%%\n", hi50, lo50);
      printf("    RT60=20ms: 125 baud %d%%\n", lo20);
      CHECK(r0==100, "no multipath: 100%");
      CHECK(lo50 > hi50 + 20, "lower baud materially improves reverb tolerance");
      CHECK(lo20 >= 80, "125 baud handles RT60=20ms (>=80%)");
    }

    printf("\n%s (%d failure%s)\n", g_fail?"CHANNEL TESTS FAILED":"ALL CHANNEL TESTS PASSED",
           g_fail, g_fail==1?"":"s");
    return g_fail?1:0;
}
