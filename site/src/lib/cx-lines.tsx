import type {ReactNode} from "react";

import {tokenizeCx} from "./cx-syntax.mjs";

export function cxLines(source: string) {
    const lines: ReactNode[][] = [[]];

    tokenizeCx(source).forEach((token, index) => {
        token.text.split("\n").forEach((text, part) => {
            if (part > 0) {
                lines.push([]);
            }
            if (!text) {
                return;
            }
            lines[lines.length - 1].push(
                token.kind ? (
                    <span className={`cx-token-${token.kind}`} key={`${index}-${part}`}>
                        {text}
                    </span>
                ) : (
                    text
                ),
            );
        });
    });

    return lines;
}
