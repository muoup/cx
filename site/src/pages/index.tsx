import {useState, type ReactNode} from "react";
import clsx from "clsx";
import Link from "@docusaurus/Link";
import Layout from "@theme/Layout";
import Heading from "@theme/Heading";

import Foliage from "../components/Foliage";
import {cxLines} from "../lib/cx-lines";
import styles from "./index.module.css";

const leakingServer = `import std::{optional, span} as std;
import std::net::{tcp, address} as std::net;

void serve(std::net::tcp_listener& server) {
    while (true) {
        std::net::tcp_stream client = server
            |> std::net::tcp_listener::accept()
            |> std::opt::unwrap_or_else(.{ continue; });

        std::span<const u8> reply = std::span::str_as_bytes("hello\\n");
        client |> std::net::tcp_stream::write(reply)
            |> std::opt::unwrap_or_else(.{ continue; });

        move client |> std::net::tcp_stream::close();
    }
}`;

const fixedServer = `import std::{optional, span} as std;
import std::net::{tcp, address} as std::net;

void serve(std::net::tcp_listener& server) {
    while (true) {
        std::net::tcp_stream client = server
            |> std::net::tcp_listener::accept()
            |> std::opt::unwrap_or_else(.{ continue; });

        std::span<const u8> reply = std::span::str_as_bytes("hello\\n");
        client |> std::net::tcp_stream::write(reply)
            |> std::opt::unwrap_or_else(.{
                move client |> std::net::tcp_stream::close();
                continue;
            });

        move client |> std::net::tcp_stream::close();
    }
}`;

type Principle = {
    title: string;
    body: ReactNode;
    files: {name: string; code: string}[];
    link?: {to: string; label: string};
    placeholder?: boolean;
};

const principles: Principle[] = [
    {
        title: "Linear resources",
        body: (
            <>
                <p>
                    CX has no destructors and no garbage collector. A type marked <code>@nodrop</code> cannot
                    quietly fall out of scope: each value must be moved somewhere else or handed to a function
                    that releases it.
                </p>
                <p>
                    Cleanup is an ordinary call you can read in the source, and the compiler rejects any path
                    that skips it, including early returns and error branches.
                </p>
            </>
        ),
        files: [
            {
                name: "string.cx",
                code: `struct string : @nodrop {
    char* data;
    usize length;
    usize capacity;
};

void string::drop(string this) {
    free(this.data);
    @leak(this);
}

void use_string(string s) {
    std::print(s |> std::string::as_str());
    move s |> string::drop();
}`,
            },
        ],
        link: {to: "/docs/manual/linear-resources", label: "Chapter 7: Linear Resources"},
    },
    {
        title: "Modern features",
        body: (
            <>
                <p>
                    Tagged unions are declared with <code>enum union</code> and taken apart with
                    exhaustive <code>match</code> statements, so adding a variant surfaces every place that
                    needs to handle it.
                </p>
                <p>
                    Templates cover generic functions and types under a one-symbol, one-definition rule. There
                    is no partial specialization, so a call always resolves to one definition you can find.
                </p>
            </>
        ),
        files: [
            {
                name: "shape.cx",
                code: `enum union shape {
    circle :: f64,
    rectangle :: struct { f64 width; f64 height; },
    point :: void
};

float get_area(shape& s) {
    match (s) {
        shape::circle(radius) => return radius * radius * 3.14;
        shape::rectangle(r) => return r.width * r.height;
        shape::point() => return 0;
    }
}`,
            },
        ],
        link: {to: "/docs/manual/tagged-unions", label: "Chapter 4: Tagged Unions"},
    },
    {
        title: "Safe subset",
        body: (
            <>
                <p>
                    Placeholder: what marking a function <code>safe</code> rules out, and how the compiler
                    checks it.
                </p>
                <p>Placeholder: when code steps outside the safe subset, and how that is made visible.</p>
            </>
        ),
        files: [{name: "safe.cx", code: "// Placeholder: a short safe function example."}],
        placeholder: true,
    },
    {
        title: "C interop",
        body: (
            <>
                <p>
                    Most C code compiles as CX with the same semantics, so an existing codebase can move over
                    one file at a time.
                </p>
                <p>
                    Going the other way, a <code>.cxh</code> entry file builds into an object file and a
                    generated C header. C programs link against CX code with no bindings layer.
                </p>
            </>
        ),
        files: [
            {
                name: "mathlib.cxh",
                code: `i32 add(i32 a, i32 b) {
    return a + b;
}`,
            },
            {
                name: "main.c",
                code: `#include <stdio.h>
#include "mathlib.h"

int main(void) {
    printf("3 + 4 = %d\\n", add(3, 4));
}`,
            },
        ],
        link: {to: "/docs/getting-started/c-interop", label: "Libraries and C Interop"},
    },
];

const standing: {title: string; items: ReactNode[]}[] = [
    {
        title: "Works today",
        items: [
            "Cranelift code generation, the default backend",
            <><code>cx init</code>, <code>cx build</code>, and <code>cx.toml</code> projects</>,
            "Linear resources, tagged unions, and templates",
            "Most C99 code, compiled unchanged",
        ],
    },
    {
        title: "Not there yet",
        items: [
            "Full C99 coverage",
            "Safe functions and contracts, both in progress",
            "Stable template syntax; type bounds are planned",
            "Stability guarantees between releases",
        ],
    },
];

const installCommands = ["git clone https://github.com/muoup/cx.git", "cd cx && cargo build --release"];

function CopyButton({text}: {text: string}) {
    const [copied, setCopied] = useState(false);

    async function copy() {
        if (typeof navigator === "undefined" || !navigator.clipboard) {
            return;
        }

        await navigator.clipboard.writeText(text);
        setCopied(true);
        window.setTimeout(() => setCopied(false), 1500);
    }

    return (
        <button className={styles.copyButton} onClick={copy} type="button">
            {copied ? "Copied" : "Copy"}
        </button>
    );
}

function Install() {
    return (
        <div className={styles.install}>
            <div className={styles.installHead}>
                <span>Build from source</span>
                <CopyButton text={installCommands.join("\n")} />
            </div>
            <pre>
                {installCommands.map((command) => (
                    <span key={command}>
                        <span className={styles.prompt}>$ </span>
                        {command}
                        {"\n"}
                    </span>
                ))}
            </pre>
        </div>
    );
}

function Code({source, errorLine}: {source: string; errorLine?: number}) {
    const lines = cxLines(source);

    return (
        <pre className={clsx("cx-code-block", lines.length === 1 && "cx-code-block--single", styles.code)}>
            <code className="cx-code-lines">
                {lines.map((line, index) => (
                    <span
                        className={clsx("cx-line", index + 1 === errorLine && "cx-line-error")}
                        data-n={index + 1}
                        key={index}
                    >
                        {line}
                        {"\n"}
                    </span>
                ))}
            </code>
        </pre>
    );
}

function LeakDiagnostic() {
    const gutter = (text: string) => <span className={styles.gutter}>{text}</span>;

    return (
        <div className={styles.diagnostic}>
            <span className={styles.error}>error:</span> `client` is marked @nodrop but is leaked without cleanup{"\n"}
            {"  "}{gutter("-->")} server.cx:12:44{"\n"}
            {"   "}{gutter("|")}{"\n"}
            {gutter("12 |")}{"             |> std::opt::unwrap_or_else(.{ continue; });\n"}
            {"   "}{gutter("|")}{" ".repeat(44)}
            <span className={styles.error}>^^^^^^^^ `client` leaks here</span>{"\n"}
            {"   "}{gutter("=")} <span className={styles.help}>help:</span> close it first: move client |&gt; std::net::tcp_stream::close();
        </div>
    );
}

function Example() {
    const [fixed, setFixed] = useState(false);

    return (
        <div className={styles.example}>
            <div className={styles.frame}>
                <div className={styles.frameHead}>
                    <span>server.cx</span>
                    <div className={styles.pills} role="group" aria-label="Example version">
                        <button aria-pressed={!fixed} onClick={() => setFixed(false)} type="button">
                            Leaks
                        </button>
                        <button aria-pressed={fixed} onClick={() => setFixed(true)} type="button">
                            Fixed
                        </button>
                    </div>
                </div>
                {fixed ? <Code source={fixedServer} /> : <Code source={leakingServer} errorLine={12} />}
            </div>
            {fixed ? (
                <div className={clsx(styles.diagnostic, styles.ok)}>
                    <span className={styles.help}>ok:</span> server.cx compiles; every path consumes `client`
                </div>
            ) : (
                <LeakDiagnostic />
            )}
        </div>
    );
}

function Hero() {
    return (
        <section className={styles.hero}>
            <div className={styles.heroGrid}>
                <div>
                    <Heading as="h1" className={styles.title}>
                        Low-level control for safe, traceable, and performant systems.
                    </Heading>
                    <p className={styles.lede}>
                        CX is an experimental systems language built as a superset of C. There are no
                        implicit destructors and no hidden control flow: every resource you acquire is
                        released in code you can read, and the compiler checks that you did.
                    </p>
                    <Install />
                    <div className={styles.actions}>
                        <Link className={styles.button} to="/docs/getting-started">
                            Getting started
                        </Link>
                        <Link to="/docs/manual/overview">Read the manual</Link>
                        <Link to="https://github.com/muoup/cx">GitHub</Link>
                    </div>
                </div>
                <Example />
            </div>
            <a className={styles.scrollHint} href="#principles">
                Principles
                <span className={styles.scrollPipe} aria-hidden="true">
                    |&gt;
                </span>
            </a>
        </section>
    );
}

function Eyebrow({children}: {children: ReactNode}) {
    return (
        <Heading as="h2" className={styles.eyebrow}>
            <span className={styles.pipe}>|&gt;</span>
            {children}
        </Heading>
    );
}

function Principles() {
    return (
        <section className={styles.section} id="principles">
            <Eyebrow>Principles</Eyebrow>
            {principles.map(({title, body, files, link, placeholder}) => (
                <article className={clsx(styles.principle, placeholder && styles.placeholder)} key={title}>
                    <div className={styles.principleText}>
                        <h3>{title}</h3>
                        {body}
                        {link && (
                            <Link className={styles.more} to={link.to}>
                                {link.label} →
                            </Link>
                        )}
                    </div>
                    <div className={styles.principleCode}>
                        {files.map(({name, code}) => (
                            <div className={styles.frame} key={name}>
                                <div className={styles.frameHead}>{name}</div>
                                <Code source={code} />
                            </div>
                        ))}
                    </div>
                </article>
            ))}
        </section>
    );
}

function Standing() {
    return (
        <section className={clsx(styles.section, styles.standingSection)} id="status">
            <div className={styles.standingIntro}>
                <Eyebrow>Where it stands</Eyebrow>
                <p>
                    CX is a research preview. The compiler builds working programs, but the language and
                    standard library are still changing.
                </p>
                <Link className={styles.more} to="/docs/getting-started/status">
                    Full project status →
                </Link>
            </div>
            <div className={styles.standing}>
                {standing.map(({title, items}) => (
                    <div key={title}>
                        <h3>{title}</h3>
                        <ul>
                            {items.map((item, index) => (
                                <li key={index}>{item}</li>
                            ))}
                        </ul>
                    </div>
                ))}
            </div>
        </section>
    );
}

export default function Home(): ReactNode {
    return (
        <Layout
            title="The CX Programming Language"
            description="An experimental systems language built as a superset of C, with linear resources and no hidden control flow."
            wrapperClassName="landing-page"
        >
            <Foliage full />
            <main className={styles.page}>
                <Hero />
                <div className={styles.sheet}>
                    <Principles />
                    <Standing />
                </div>
            </main>
        </Layout>
    );
}
