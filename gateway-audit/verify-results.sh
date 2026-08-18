#!/bin/sh
set -eu

if [ "$#" -lt 2 ] || [ "$#" -gt 3 ]; then
    echo "Usage: $0 <results.txt> <expectations.tsv> [report.md]" >&2
    exit 1
fi

RESULTS=$1
EXPECTATIONS=$2
REPORT=${3:-report.md}
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
        if ($3 != "failing" && $3 != "deferred") {
            print "Invalid expectation disposition for suite " $1 ": " $3 > "/dev/stderr"
            violations++
        }
        count = split($2, positions, ",")
        for (item = 1; item <= count; item++) {
            if (positions[item] !~ /^[0-9]+$/) {
                print "Invalid expected case index for suite " $1 ": " positions[item] > "/dev/stderr"
                violations++
            }
            case_id = $1 "_" positions[item]
            if (case_id in expected) {
                print "Duplicate audit expectation: " case_id > "/dev/stderr"
                violations++
            }
            disposition[case_id] = $3
            owner[case_id] = $4
            rationale[case_id] = $5
            expected[case_id] = 1
        }
        next
    }

    /^[a-z0-9-]+$/ {
        current = $0
        next
    }

    /^[.X]+$/ && current != "" {
        for (position = 1; position <= length($0); position++) {
            case_id = current "_" (position - 1)
            marker = substr($0, position, 1)
            result = marker == "." ? "pass" : "fail"
            observed[case_id] = 1

            if (case_id in expected) {
                case_disposition = disposition[case_id]
                case_owner = owner[case_id]
                case_rationale = rationale[case_id]
            } else {
                case_disposition = "supported"
                case_owner = "-"
                case_rationale = "Passing behavior is required."
            }
            print "| `" case_id "` | " result " | " case_disposition " | " case_owner " | " case_rationale " |" >> report

            total++
            counts[case_disposition]++
            if (marker == "X") failures++
            if (marker == "X" && !(case_id in expected)) {
                print "Unexpected audit case failure: " case_id > "/dev/stderr"
                violations++
            }
            if (marker == "." && case_id in expected) {
                print "Expected audit failure passed; remove its exception: " case_id > "/dev/stderr"
                violations++
            }
        }
        current = ""
    }

    END {
        for (case_id in expected) {
            if (!(case_id in observed)) {
                print "Expected audit case was not reported: " case_id > "/dev/stderr"
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
' "$EXPECTATIONS" "$RESULTS"

cat "$REPORT"
