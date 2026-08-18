#!/bin/sh
set -eu

if [ "$#" -lt 3 ] || [ "$#" -gt 4 ]; then
    echo "Usage: $0 <results.txt> <expectations.tsv> <supported-cases.txt> [report.md]" >&2
    exit 1
fi

RESULTS=$1
EXPECTATIONS=$2
SUPPORTED_CASES=$3
REPORT=${4:-report.md}
REVISION=${FEDERATION_GATEWAY_AUDIT_REVISION:-unknown}

{
    echo "# Caliban Federation Gateway Audit"
    echo
    echo "Upstream revision: \`$REVISION\`"
    echo
    echo "| Case | Result | Disposition | Owner | Rationale |"
    echo "| --- | --- | --- | --- | --- |"
} > "$REPORT"

awk -F '\t' -v report="$REPORT" '
    FILENAME == ARGV[1] {
        if (FNR == 1) next
        disposition[$1] = $2
        owner[$1] = $3
        rationale[$1] = $4
        expected[$1] = 1
        next
    }

    FILENAME == ARGV[2] {
        supported[$1] = 1
        next
    }

    /^[a-z0-9-]+$/ {
        current = $0
        next
    }

    /^[.X]+$/ && current != "" {
        if (!(current in expected)) {
            print "Unclassified upstream suite: " current > "/dev/stderr"
            violations++
            current = ""
            next
        }

        seen[current] = 1
        for (position = 1; position <= length($0); position++) {
            case_id = current "_" (position - 1)
            marker = substr($0, position, 1)
            result = marker == "." ? "pass" : "fail"
            if (case_id in supported) {
                case_disposition = "supported"
                case_owner = "ticket-10"
                case_rationale = "Pinned passing behavior; a failure is a regression."
                supported_seen[case_id] = 1
            } else {
                case_disposition = disposition[current]
                case_owner = owner[current]
                case_rationale = rationale[current]
            }
            print "| `" case_id "` | " result " | " case_disposition " | " case_owner " | " case_rationale " |" >> report

            total++
            counts[case_disposition]++
            if (marker == "X") failures++
            if (case_id in supported && marker == "X") {
                print "Supported audit case failed: " case_id > "/dev/stderr"
                violations++
            }
            if (!(case_id in supported) && marker == ".") {
                print "Passing audit case is not in the supported baseline: " case_id > "/dev/stderr"
                violations++
            }
        }
        current = ""
    }

    END {
        for (suite in expected) {
            if (!(suite in seen)) {
                print "Expected upstream suite was not reported: " suite > "/dev/stderr"
                violations++
            }
        }
        for (case_id in supported) {
            if (!(case_id in supported_seen)) {
                print "Supported audit case was not reported: " case_id > "/dev/stderr"
                violations++
            }
        }

        print "" >> report
        print "Total cases: " total >> report
        print "" >> report
        print "- Supported: " (counts["supported"] + 0) >> report
        print "- Owned breadth failures: " (counts["failing"] + 0) >> report
        print "- Explicitly deferred: " (counts["deferred"] + 0) >> report
        print "- Observed failures: " (failures + 0) >> report

        if (violations > 0) exit 1
    }
' "$EXPECTATIONS" "$SUPPORTED_CASES" "$RESULTS"

cat "$REPORT"
