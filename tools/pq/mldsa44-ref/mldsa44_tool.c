/* mldsa44_tool.c: a keypair and signatures from the pq-crystals ML-DSA-44 reference implementation,
 * for the feed battery: a source signs its figures with it and the desk verifies them.
 *
 *   mldsa44_tool keygen <seedhex32>                 -> pk hex, sk hex (deterministic from the seed)
 *   mldsa44_tool sign <skhex> <ctx> <messagefile>   -> signature hex (2,420 bytes)
 *
 * Built by tools/pq/mldsa44-ref/build.sh against the reference sources (DILITHIUM_REF) with DILITHIUM_MODE=2.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "api.h"
#include "randombytes.h"

static int hexval(char c) { return (c >= '0' && c <= '9') ? c - '0' : (c >= 'a' && c <= 'f') ? c - 'a' + 10 : (c >= 'A' && c <= 'F') ? c - 'A' + 10 : -1; }
static size_t unhex(const char *s, unsigned char *out, size_t max) {
  size_t n = strlen(s) / 2;
  if (n > max) return 0;
  for (size_t i = 0; i < n; i++) { int a = hexval(s[2 * i]), b = hexval(s[2 * i + 1]); if (a < 0 || b < 0) return 0; out[i] = (unsigned char)(a * 16 + b); }
  return n;
}
static void phex(const unsigned char *b, size_t n) { for (size_t i = 0; i < n; i++) printf("%02x", b[i]); printf("\n"); }

/* the reference's randombytes is replaced by a seed-driven stream so keygen is reproducible */
static unsigned char g_seed[32]; static size_t g_used = 0;
void randombytes(uint8_t *out, size_t outlen) {
  for (size_t i = 0; i < outlen; i++) { out[i] = g_seed[(g_used + i) % 32] ^ (unsigned char)((g_used + i) * 131); }
  g_used += outlen;
}

int main(int argc, char **argv) {
  if (argc >= 3 && strcmp(argv[1], "keygen") == 0) {
    if (unhex(argv[2], g_seed, 32) != 32) { fprintf(stderr, "seed: 32 bytes hex\n"); return 2; }
    unsigned char pk[pqcrystals_dilithium2_PUBLICKEYBYTES], sk[pqcrystals_dilithium2_SECRETKEYBYTES];
    if (pqcrystals_dilithium2_ref_keypair(pk, sk) != 0) return 3;
    phex(pk, sizeof pk); phex(sk, sizeof sk);
    return 0;
  }
  if (argc >= 5 && strcmp(argv[1], "sign") == 0) {
    unsigned char sk[pqcrystals_dilithium2_SECRETKEYBYTES];
    if (unhex(argv[2], sk, sizeof sk) != sizeof sk) { fprintf(stderr, "sk: %d bytes hex\n", (int)sizeof sk); return 2; }
    const char *ctx = argv[3];
    FILE *f = fopen(argv[4], "rb"); if (!f) { perror("message"); return 2; }
    fseek(f, 0, SEEK_END); long n = ftell(f); fseek(f, 0, SEEK_SET);
    unsigned char *m = malloc(n > 0 ? n : 1); if (fread(m, 1, n, f) != (size_t)n) return 2; fclose(f);
    unsigned char sig[pqcrystals_dilithium2_BYTES]; size_t siglen = 0;
    memset(g_seed, 0x5a, 32);
    if (pqcrystals_dilithium2_ref_signature(sig, &siglen, m, (size_t)n, (const uint8_t *)ctx, strlen(ctx), sk) != 0) return 3;
    phex(sig, siglen);
    return 0;
  }
  fprintf(stderr, "usage: mldsa44_tool keygen <seedhex32> | sign <skhex> <ctx> <messagefile>\n");
  return 1;
}
