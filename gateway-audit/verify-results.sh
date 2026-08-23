#!/bin/sh
set -eu

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <results.txt>" >&2
    exit 1
fi

RESULTS=$1

awk -F '\t' '
    /^[a-z0-9-]+$/ {
        current = $0
        next
    }

    /^[.X]+$/ && current != "" {
        for (position = 1; position <= length($0); position++) {
            case_id = current "_" (position - 1)
            marker = substr($0, position, 1)
            if (case_id in observed) {
                print "Duplicate audit case: " case_id > "/dev/stderr"
                violations++
            }
            observed[case_id] = 1

            total++
            if (marker == "X") {
                failures++
                print "Audit case failed: " case_id > "/dev/stderr"
                violations++
            }
        }
        current = ""
    }

    /^Total: [0-9]+$/ { sub(/^Total: /, ""); expected_total = $0; next }
    /^Passed: [0-9]+$/ { sub(/^Passed: /, ""); expected_passed = $0; next }
    /^Failed: [0-9]+$/ { sub(/^Failed: /, ""); expected_failed = $0; summary_seen = 1; next }

    END {
        if (!summary_seen || total == 0 || expected_total + 0 != total + 0 || expected_passed + 0 != total - failures || expected_failed + 0 != failures + 0) {
            print "Audit result summary is missing or inconsistent." > "/dev/stderr"
            violations++
        }
        if (violations > 0) exit 1
        print "Federation Gateway Audit: " total "/" total " cases passed."
    }
' "$RESULTS"
