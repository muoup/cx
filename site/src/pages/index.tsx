import {useState, type ReactNode} from "react";
import clsx from "clsx";
import Link from "@docusaurus/Link";
import Layout from "@theme/Layout";
import Heading from "@theme/Heading";

import Foliage from "../components/Foliage";
import {chlLines} from "../lib/chl-lines";
import styles from "./index.module.css";

const leakingServer = `import std::{memory::span, net::tcp, result, string} as std;

void serve(std::tcp_server& server) {
    while (true) {
        std::tcp_connection client = server
            |> std::tcp_server::accept()
            |> std::result::unwrap_or(emit .{ continue; })!;

        std::span(const u8) reply = std::str::as_const_bytes("hello\\n");
        client |> std::tcp_connection::send(reply)
            |> std::result::unwrap_or(emit .{ continue; })!;

        move client |> std::tcp_connection::drop();
    }
}`;

const fixedServer = `import std::{memory::span, net::tcp, result, string} as std;

void serve(std::tcp_server& server) {
    while (true) {
        std::tcp_connection client = server
            |> std::tcp_server::accept()
            |> std::result::unwrap_or(emit .{ continue; })!;

        std::span(const u8) reply = std::str::as_const_bytes("hello\\n");
        client |> std::tcp_connection::send(reply)
            |> std::result::unwrap_or(emit .{
                move client |> std::tcp_connection::drop();
                continue;
            })!;

        move client |> std::tcp_connection::drop();
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
                    Chloro gives the tools to write safe code with no hidden behavior, no destructors, no garbage collector, everything is manually handled but rules are well-enforced.
                </p>
                <p>
                    Use `@nodrop` to define a resource that must be cleaned up before they are dropped, requiring an explicit leak. All cleanup is done through standard explicit methods, and the compiler enforced that resources are never leaked unexpectedly.
                </p>
            </>
        ),
        files: [
            {
                name: "string.chl",
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
                    Chloro takes a modest set of modern language syntax including modules, tagged unions, and pattern matching to enable safe and expressive programming with a smaller language core in favor of an extensive comptime for type-safe meta code generation.
                </p>
                <p>
                    Features like member functions, operator overloading, and template specialization are intentionally excluded, every symbol corresponds to a single well-defined meaning, and hidden behavior is avoided in favor of explicit and readable idioms.
                </p>
            </>
        ),
        files: [
            {
                name: "shape.chl",
                code: `enum union shape {
    circle :: f64,
    rectangle :: struct { f64 width; f64 height; },
    point :: void
};

float get_area(shape& s) {
    match (s) {
        circle(f64 radius) => return radius * radius * 3.14;
        rectangle(auto r) => return r.width * r.height;
        point() => return 0;
    }
}`,
            },
        ],
        link: {to: "/docs/manual/tagged-unions", label: "Chapter 4: Tagged Unions"},
    },
    // {
    //     title: "Safe subset",
    //     body: (
    //         <>
    //             <p>
    //                 Placeholder: what marking a function <code>safe</code> rules out, and how the compiler
    //                 checks it.
    //             </p>
    //             <p>Placeholder: when code steps outside the safe subset, and how that is made visible.</p>
    //         </>
    //     ),
    //     files: [{name: "safe.chl", code: "// Placeholder: a short safe function example."}],
    //     placeholder: true,
    // },
    {
        title: "C Compatibility",
        body: (
            <>
                <p>
                    The Chloro compiler is an in-progress drop-in replacement for compilers like gcc and clang, and due to the backward compatibility of the language, any Chloro file can directly include a C header and call C functions without any wrappers or bindings.
                </p>
                <p>
                    The build system of the compiler is also designed with first-class support for C interoperability. Ordinary '.chl' modules can expose public extern C definitions through generated C headers, while remaining available through normal module imports. Library targets select their header modules in 'chl.lua'.
                </p>
            </>
        ),
        files: [
            {
                name: "mathlib.chl",
                code: `public extern "C":

i32 add(i32 a, i32 b) {
    return a + b;
}`,
            },
            {
                name: "chl.lua",
                code: `package("mathlib")
stdlib("std")

local lib = target("mathlib", "library")
lib:match("mathlib.chl")
lib:headers("mathlib.chl")`,
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
            <><code>chl init</code>, <code>chl build</code>, and <code>chl.lua</code> projects</>,
            "Linear resources, tagged unions, and templates",
            "Most C99 code, compiled unchanged",
        ],
    },
    {
        title: "Not there yet",
        items: [
            "Full C99 coverage",
            "Safe functions and contracts",
            "Lifetime tracking",
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
    const lines = chlLines(source);

    return (
        <pre className={clsx("chl-code-block", lines.length === 1 && "chl-code-block--single", styles.code)}>
            <code className="chl-code-lines">
                {lines.map((line, index) => (
                    <span
                        className={clsx("chl-line", index + 1 === errorLine && "chl-line-error")}
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
            {"  "}{gutter("-->")} server.chl:12:44{"\n"}
            {"   "}{gutter("|")}{"\n"}
            {gutter("12 |")}{"             |> std::result::unwrap_or(emit .{ continue; })!;\n"}
            {"   "}{gutter("|")}{" ".repeat(44)}
            <span className={styles.error}>^^^^^^^^ `client` leaks here</span>{"\n"}
            {"   "}{gutter("=")} <span className={styles.help}>help:</span> close it first: move client |&gt; std::tcp_connection::drop();
        </div>
    );
}

function Example() {
    const [fixed, setFixed] = useState(false);

    return (
        <div className={styles.example}>
            <div className={styles.frame}>
                <div className={styles.frameHead}>
                    <span>server.chl</span>
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
                    <span className={styles.help}>ok:</span> server.chl compiles; every path consumes `client`
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
                        Chloro is an experimental systems language taking an alternative approach to modernizing C. All code is explicit, every reference is easily traced to its definition, and the compiler provides safety measures you can gradually adopt to ensure your codebase does what you expect.
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
                    Chloro is a research preview. The compiler builds working programs, but the language and
                    standard library are still changing.
                </p>
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
            title="The Chloro Programming Language"
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
