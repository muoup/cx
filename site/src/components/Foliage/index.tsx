import type {CSSProperties, ReactNode} from "react";

import {foliageSymbols} from "./symbols";
import styles from "./styles.module.css";

type Vars = CSSProperties & Record<`--${string}`, string>;

const leaves: Vars[] = [
    {"--y": "11%", "--d": "29s", "--delay": "-14s", "--dy": "40px", "--s": "23px", "--sw": "2.9s", "--r0": "11deg", "--r1": "115deg"},
    {"--y": "23%", "--d": "30s", "--delay": "-3s", "--dy": "-13px", "--s": "22px", "--sw": "2.7s", "--r0": "20deg", "--r1": "109deg"},
    {"--y": "38%", "--d": "34s", "--delay": "-33s", "--dy": "77px", "--s": "22px", "--sw": "3.6s", "--r0": "-55deg", "--r1": "33deg"},
    {"--y": "43%", "--d": "34s", "--delay": "-30s", "--dy": "63px", "--s": "20px", "--sw": "3.8s", "--r0": "-52deg", "--r1": "1deg"},
    {"--y": "58%", "--d": "28s", "--delay": "-7s", "--dy": "-15px", "--s": "27px", "--sw": "2.6s", "--r0": "15deg", "--r1": "118deg"},
    {"--y": "77%", "--d": "25s", "--delay": "-25s", "--dy": "-69px", "--s": "24px", "--sw": "3.1s", "--r0": "-25deg", "--r1": "51deg"},
    {"--y": "83%", "--d": "27s", "--delay": "-21s", "--dy": "12px", "--s": "21px", "--sw": "4.0s", "--r0": "-24deg", "--r1": "27deg"},
];

const sweep = "M2 16 C 70 8, 140 22, 210 14 S 300 6, 330 12";
const curl = "M2 14 C 60 6, 120 20, 190 12 S 262 2, 276 10 C 284 16, 272 24, 264 16";

const gusts: {path: string; style: Vars}[] = [
    {path: sweep, style: {"--x": "60%", "--y": "13%", "--w": "260px", "--d": "11s", "--delay": "-2.4s"}},
    {path: curl, style: {"--x": "7%", "--y": "24%", "--w": "350px", "--d": "15s", "--delay": "-5.8s"}},
    {path: "M2 12 C 90 20, 170 4, 250 12 S 350 18, 390 10", style: {"--x": "41%", "--y": "42%", "--w": "294px", "--d": "9s", "--delay": "-3.2s"}},
    {path: "M2 18 C 50 10, 110 22, 170 14 S 228 4, 240 12 C 248 18, 236 26, 228 18", style: {"--x": "5%", "--y": "54%", "--w": "378px", "--d": "11s", "--delay": "-1.3s"}},
    {path: sweep, style: {"--x": "10%", "--y": "66%", "--w": "362px", "--d": "10s", "--delay": "-5.1s"}},
    {path: curl, style: {"--x": "15%", "--y": "85%", "--w": "353px", "--d": "13s", "--delay": "-10.1s"}},
];

function Bush({symbol, className}: {symbol: string; className: string}) {
    return (
        <svg className={`${styles.bush} ${className}`}>
            <use href={`#${symbol}`} />
        </svg>
    );
}

function Wind() {
    return (
        <div className={styles.wind}>
            {leaves.map((style, index) => (
                <i key={index} style={style}>
                    <svg>
                        <use href="#drift-leaf" />
                    </svg>
                </i>
            ))}
            {gusts.map(({path, style}, index) => (
                <svg className={styles.gust} key={index} style={style} viewBox="0 0 400 28">
                    <path d={path} pathLength={100} />
                </svg>
            ))}
        </div>
    );
}

export default function Foliage({full = false}: {full?: boolean}): ReactNode {
    return (
        <div className={styles.foliage} aria-hidden="true">
            <svg className={styles.symbols} dangerouslySetInnerHTML={{__html: foliageSymbols}} />
            {full ? (
                <>
                    <Bush symbol="bush-b" className={styles.topLeft} />
                    <Bush symbol="bush-a" className={styles.topRight} />
                    <Bush symbol="bush-a" className={styles.bottomLeft} />
                    <Bush symbol="bush-b" className={styles.bottomRight} />
                    <Wind />
                </>
            ) : (
                <Bush symbol="bush-b" className={`${styles.bottomRight} ${styles.small}`} />
            )}
        </div>
    );
}
