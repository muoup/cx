import {useEffect, useRef, type CSSProperties, type ReactNode} from "react";

import {bushes, foliageSymbols} from "./symbols";
import styles from "./styles.module.css";

type Vars = CSSProperties & Record<`--${string}`, string>;

const farLeaves: Vars[] = [
    {"--y": "11%", "--d": "29s", "--delay": "-14s", "--dy": "40px", "--s": "23px", "--sw": "2.9s", "--r0": "11deg", "--r1": "115deg"},
    {"--y": "23%", "--d": "30s", "--delay": "-3s", "--dy": "-13px", "--s": "22px", "--sw": "2.7s", "--r0": "20deg", "--r1": "109deg"},
    {"--y": "38%", "--d": "34s", "--delay": "-33s", "--dy": "77px", "--s": "22px", "--sw": "3.6s", "--r0": "-55deg", "--r1": "33deg"},
    {"--y": "43%", "--d": "34s", "--delay": "-30s", "--dy": "63px", "--s": "20px", "--sw": "3.8s", "--r0": "-52deg", "--r1": "1deg"},
    {"--y": "58%", "--d": "28s", "--delay": "-7s", "--dy": "-15px", "--s": "27px", "--sw": "2.6s", "--r0": "15deg", "--r1": "118deg"},
    {"--y": "77%", "--d": "25s", "--delay": "-25s", "--dy": "-69px", "--s": "24px", "--sw": "3.1s", "--r0": "-25deg", "--r1": "51deg"},
    {"--y": "83%", "--d": "27s", "--delay": "-21s", "--dy": "12px", "--s": "21px", "--sw": "4.0s", "--r0": "-24deg", "--r1": "27deg"},
];

const nearLeaves: Vars[] = [
    {"--y": "17%", "--d": "19s", "--delay": "-4s", "--dy": "90px", "--s": "52px", "--sw": "3.4s", "--r0": "-30deg", "--r1": "80deg"},
    {"--y": "34%", "--d": "22s", "--delay": "-16s", "--dy": "-60px", "--s": "46px", "--sw": "3.9s", "--r0": "10deg", "--r1": "130deg"},
    {"--y": "52%", "--d": "17s", "--delay": "-9s", "--dy": "120px", "--s": "58px", "--sw": "3.1s", "--r0": "-60deg", "--r1": "20deg"},
    {"--y": "69%", "--d": "21s", "--delay": "-13s", "--dy": "-40px", "--s": "48px", "--sw": "4.2s", "--r0": "25deg", "--r1": "110deg"},
    {"--y": "88%", "--d": "18s", "--delay": "-1s", "--dy": "-110px", "--s": "54px", "--sw": "3.6s", "--r0": "-15deg", "--r1": "70deg"},
];

const sweep = "M2 16 C 70 8, 140 22, 210 14 S 300 6, 330 12";
const curl = "M2 14 C 60 6, 120 20, 190 12 S 262 2, 276 10 C 284 16, 272 24, 264 16";

const farGusts: {path: string; style: Vars}[] = [
    {path: sweep, style: {"--x": "60%", "--y": "13%", "--w": "260px", "--d": "11s", "--delay": "-2.4s"}},
    {path: curl, style: {"--x": "7%", "--y": "24%", "--w": "350px", "--d": "15s", "--delay": "-5.8s"}},
    {path: "M2 12 C 90 20, 170 4, 250 12 S 350 18, 390 10", style: {"--x": "41%", "--y": "42%", "--w": "294px", "--d": "9s", "--delay": "-3.2s"}},
    {path: "M2 18 C 50 10, 110 22, 170 14 S 228 4, 240 12 C 248 18, 236 26, 228 18", style: {"--x": "5%", "--y": "54%", "--w": "378px", "--d": "11s", "--delay": "-1.3s"}},
    {path: sweep, style: {"--x": "10%", "--y": "66%", "--w": "362px", "--d": "10s", "--delay": "-5.1s"}},
    {path: curl, style: {"--x": "15%", "--y": "85%", "--w": "353px", "--d": "13s", "--delay": "-10.1s"}},
];

const nearGusts: {path: string; style: Vars}[] = [
    {path: curl, style: {"--x": "52%", "--y": "20%", "--w": "720px", "--d": "14s", "--delay": "-6.5s"}},
    {path: sweep, style: {"--x": "-4%", "--y": "47%", "--w": "760px", "--d": "12s", "--delay": "-0.8s"}},
    {path: curl, style: {"--x": "38%", "--y": "76%", "--w": "700px", "--d": "16s", "--delay": "-11s"}},
];

function Bush({art, className}: {art: keyof typeof bushes; className: string}) {
    return <svg className={`${styles.bush} ${className}`} viewBox="0 0 320 320" dangerouslySetInnerHTML={{__html: `<g>${bushes[art]}</g>`}} />;
}

function Wind({leaves, gusts, className}: {leaves: Vars[]; gusts: typeof farGusts; className: string}) {
    return (
        <div className={`${styles.wind} ${className}`}>
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

type Spring = {
    el: SVGGElement;
    transform: string;
    parent?: Spring;
    angle: number;
    velocity: number;
    wind: number;
    stiffness: number;
    damping: number;
    reach: number;
    lag: number;
    flutter: number;
    phase: number;
};

// Per level (tier, clump, leaf): natural frequency range in Hz, damping ratio, and lean in degrees at full wind.
const levels = [
    {hz: [0.9, 1.2], ratio: 0.2, reach: 0.4},
    {hz: [1.5, 2.4], ratio: 0.14, reach: 2.4},
    {hz: [2.6, 4], ratio: 0.1, reach: 6},
];

function hash(n: number) {
    const x = Math.sin(n * 12.9898) * 43758.5453;
    return x - Math.floor(x);
}

function springs(bush: Element, leavesOnly: boolean, seed: number): Spring[] {
    const ids = new Map<string, Spring>();
    const parts = Array.from(bush.querySelectorAll<SVGGElement>("g[data-l]")).filter((el) => !leavesOnly || el.dataset.l === "2");
    return parts.map((el, index) => {
        const level = levels[Number(el.dataset.l)];
        const [x, y] = el.dataset.p!.split(",").map(Number);
        const random = (salt: number) => hash(seed + index * 7 + salt);
        const omega = 2 * Math.PI * (level.hz[0] + (level.hz[1] - level.hz[0]) * random(1));
        const spring: Spring = {
            el,
            transform: ` ${x} ${y})`,
            parent: el.dataset.parent ? ids.get(el.dataset.parent) : undefined,
            angle: 0,
            velocity: 0,
            wind: 0,
            stiffness: omega * omega,
            damping: 2 * level.ratio * omega,
            reach: level.reach,
            // Farther from the screen corner catches the scroll later, so the motion ripples outward.
            lag: 0.04 + (Math.hypot(x, y) / 300) * 0.22 + random(2) * 0.05,
            flutter: omega * (0.5 + random(3) * 0.2),
            phase: random(4) * 2 * Math.PI,
        };
        if (el.dataset.id) {
            ids.set(el.dataset.id, spring);
        }
        return spring;
    });
}

// Scrolling rustles the bushes: every tier, clump and leaf is a damped spring pushed by scroll speed, each with its
// own frequency and delay so they fall out of step and settle on their own.
function useRustle(leavesOnly: boolean) {
    const ref = useRef<HTMLDivElement>(null);

    useEffect(() => {
        const root = ref.current;
        if (!root || window.matchMedia("(prefers-reduced-motion: reduce)").matches) {
            return;
        }

        const parts = Array.from(root.querySelectorAll(`.${styles.bush}`)).flatMap((bush, index) => springs(bush, leavesOnly, index * 101));
        let wind = 0;
        let lastY = window.scrollY;
        let lastScroll = performance.now();
        let lastFrame = 0;
        let frame = 0;

        function step(now: number) {
            const dt = Math.min((now - lastFrame) / 1000, 1 / 30);
            const t = now / 1000;
            lastFrame = now;
            wind *= Math.exp(-dt / 0.18);

            let moving = Math.abs(wind) > 0.01;
            for (const part of parts) {
                part.wind += (wind - part.wind) * (1 - Math.exp(-dt / part.lag));
                const drive = part.wind + Math.abs(part.wind) * 0.5 * Math.sin(t * part.flutter + part.phase);
                part.velocity += (part.stiffness * (part.reach * drive - part.angle) - part.damping * part.velocity) * dt;
                part.angle = Math.max(-2 * part.reach, Math.min(part.angle + part.velocity * dt, 2 * part.reach));
                moving ||= Math.abs(part.angle) > 0.005 || Math.abs(part.velocity) > 0.05;
            }
            for (const part of parts) {
                const own = `rotate(${part.angle.toFixed(3)}${part.transform}`;
                part.el.setAttribute("transform", part.parent ? `rotate(${part.parent.angle.toFixed(3)}${part.parent.transform} ${own}` : own);
            }

            frame = moving ? requestAnimationFrame(step) : 0;
        }

        function onScroll() {
            const now = performance.now();
            const speed = (window.scrollY - lastY) / Math.min(Math.max(now - lastScroll, 16), 100);
            lastY = window.scrollY;
            lastScroll = now;
            wind += (Math.max(-1, Math.min(speed / 2.5, 1)) - wind) * 0.5;
            if (!frame) {
                lastFrame = now;
                frame = requestAnimationFrame(step);
            }
        }

        window.addEventListener("scroll", onScroll, {passive: true});
        return () => {
            window.removeEventListener("scroll", onScroll);
            cancelAnimationFrame(frame);
        };
    }, [leavesOnly]);

    return ref;
}

export default function Foliage({full = false}: {full?: boolean}): ReactNode {
    const ref = useRustle(!full);

    return (
        <div className={styles.foliage} aria-hidden="true" ref={ref}>
            <svg className={styles.symbols} dangerouslySetInnerHTML={{__html: foliageSymbols}} />
            {full ? (
                <>
                    <Bush art="b" className={styles.topLeft} />
                    <Bush art="a" className={styles.topRight} />
                    <Bush art="a" className={styles.bottomLeft} />
                    <Bush art="b" className={styles.bottomRight} />
                    <Wind leaves={farLeaves} gusts={farGusts} className={styles.far} />
                    <Wind leaves={nearLeaves} gusts={nearGusts} className={styles.near} />
                </>
            ) : (
                <Bush art="b" className={`${styles.bottomLeft} ${styles.small}`} />
            )}
        </div>
    );
}
