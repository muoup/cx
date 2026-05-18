import type {ReactNode} from 'react';
import Link from '@docusaurus/Link';
import Layout from '@theme/Layout';
import Heading from '@theme/Heading';

import styles from './index.module.css';

const featureCards = [
  {
    title: 'C-shaped by design',
    text: 'CX keeps explicit control flow, predictable layout, and familiar syntax while removing some declaration-order friction.',
  },
  {
    title: 'Ownership when it matters',
    text: '@nocopy, @nodrop, move, @leak, @unpack, and @adopt make resource behavior visible instead of ambient.',
  },
  {
    title: 'Verification opt-in',
    text: 'safe functions and contracts can be analyzed with --analysis without turning the whole language into a restricted subset.',
  },
];

function Hero() {
  return (
    <header className={styles.hero}>
      <div className={styles.heroInner}>
        <div className={styles.heroCopy}>
          <p className={styles.kicker}>Experimental systems language</p>
          <Heading as="h1" className={styles.title}>
            CX Programming Language
          </Heading>
          <p className={styles.subtitle}>
            A C-shaped language with explicit ownership, tagged unions,
            contracts, and opt-in verification for code that still wants to feel
            close to the machine.
          </p>
          <div className={styles.actions}>
            <Link className="button button--primary button--lg" to="/docs/getting-started">
              Get Started
            </Link>
            <Link className="button button--secondary button--lg" to="/docs/manual/">
              Language Manual
            </Link>
          </div>
        </div>
        <div className={styles.codePanel} aria-label="CX code example">
          <div className={styles.codeChrome}>
            <span />
            <span />
            <span />
          </div>
          <pre>
            <code>{`struct Resource : @nodrop {
    i32 handle;
};

void Resource::drop(this) safe {
    @unsafe {
        @leak(this);
    };
}

int main() safe {
    Resource r = { .handle = 7 };
    r.drop();
    return 0;
}`}</code>
          </pre>
        </div>
      </div>
    </header>
  );
}

function FeatureGrid() {
  return (
    <section className={styles.section}>
      <div className={styles.sectionInner}>
        <Heading as="h2" className={styles.sectionTitle}>
          Public docs for the implemented language surface
        </Heading>
        <div className={styles.grid}>
          {featureCards.map((feature) => (
            <article className={styles.feature} key={feature.title}>
              <h3>{feature.title}</h3>
              <p>{feature.text}</p>
            </article>
          ))}
        </div>
      </div>
    </section>
  );
}

function QuickLinks() {
  return (
    <section className={styles.quickLinks}>
      <div className={styles.sectionInner}>
        <Heading as="h2" className={styles.sectionTitle}>
          Start with the essentials
        </Heading>
        <div className={styles.linkList}>
          <Link
            className={styles.linkItem}
            to="/docs/getting-started">
            Build the compiler and compile your first CX project
          </Link>
          <Link className={styles.linkItem} to="/docs/manual/aggregate-types">
            Learn structs, tagged unions, and ownership attributes
          </Link>
          <Link className={styles.linkItem} to="/docs/build-system">
            Use cx.toml for project and library builds
          </Link>
        </div>
      </div>
    </section>
  );
}

export default function Home(): ReactNode {
  return (
    <Layout
      title="CX Programming Language"
      description="CX is an experimental C-like systems language with explicit ownership and opt-in verification.">
      <Hero />
      <main>
        <FeatureGrid />
        <QuickLinks />
      </main>
    </Layout>
  );
}
