import type {ReactNode} from "react";

import {tokenizeChl} from "./chl-syntax.mjs";

export function chlLines(source: string) {
    const lines: ReactNode[][] = [[]];

    tokenizeChl(source).forEach((token, index) => {
        token.text.split("\n").forEach((text, part) => {
            if (part > 0) {
                lines.push([]);
            }
            if (!text) {
                return;
            }
            lines[lines.length - 1].push(
                token.kind ? (
                    <span className={`chl-token-${token.kind}`} key={`${index}-${part}`}>
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
