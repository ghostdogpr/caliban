# 18 — Add source execution policy

**Outcome:** Applications can configure safe outbound headers, deadlines, retries, and finite source behavior through immutable source descriptions.

**Blocked by:** 17 — Complete the GraphQL-over-HTTP source boundary

**Status:** ready-for-agent

## Completion criteria

- [ ] Source configuration covers finite call timeout, request bytes, response bytes, retry count/backoff, and the concurrency value consumed by Ticket 19.
- [ ] Runtime header policy combines configured, selected incoming, and effectful headers with explicit precedence.
- [ ] Protocol-owned headers cannot be overridden accidentally and forwarding all incoming headers is an explicit opt-in.
- [ ] Retries are bounded and limited to replay-safe operations and classified failures; one logical source call owns all attempts.
- [ ] Environment requirements from effectful header policies compose through the gateway type.
- [ ] Invalid or non-finite settings fail build with accumulated source-attributed diagnostics.
- [ ] Tests cover precedence, protected headers, retry eligibility, attempt ownership, and safe logging.
