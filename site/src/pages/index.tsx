import type {ReactNode} from 'react';
import Link from '@docusaurus/Link';
import Layout from '@theme/Layout';
import Heading from '@theme/Heading';

import styles from './index.module.css';

const features = [
  {
    title: 'No Hidden Control Flow',
    description: 'CX eliminates RAII and hidden destructors. Resource cleanup is never a side effect of scope exit; it is always an explicit call. This ensures that resource lifetimes are visible and predictable in the source text.',
  },
  {
    title: 'Linear Resource Tracking',
    description: 'Types marked with @nodrop are treated as linear resources. The compiler tracks their initialization and disposal, raising a type error for any path that reaches a scope exit without discharging the resource.',
  },
  {
    title: 'Verified Safe Subset',
    description: 'The \'safe\' keyword introduces a restricted sublanguage that forbids raw pointers and unchecked memory access. Safe functions are subject to formal verification via FMIR analysis, allowing you to prove properties like memory safety and contract adherence.',
  },
  {
    title: 'C Interoperability',
    description: 'CX is designed to live within the C ecosystem. It shares the same data layout and calling conventions, making it trivial to call C functions or expose CX functions to existing C codebases.',
  },
];

function Hero() {
  return (
    <div className={styles.hero}>
      <div className={styles.heroInner}>
        <div className={styles.heroLeft}>
          <Heading as="h1" className={styles.heroTitle}>CX</Heading>
          <p className={styles.heroPhilosophy}>
            A systems programming language focused on <strong>explicit resource management</strong>, 
            <strong>linear safety</strong>, and <strong>formal verification</strong>.
          </p>
        </div>
        <div className={styles.heroRight}>
          <div className={styles.getStartedBox}>
            <div className={styles.getStartedHeader}>LATEST RELEASE: 0.1.0</div>
            <div className={styles.getStartedBody}>
              <Link className="button button--secondary button--block" to="/docs/getting-started">
                GET STARTED
              </Link>
              <div className={styles.getStartedLinks}>
                <Link to="/docs/manual/">Documentation</Link>
                <Link to="https://github.com/muoup/cx">Source</Link>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

function MainContent() {
  return (
    <div className={styles.mainGrid}>
      <div className={styles.featuresColumn}>
        {features.map((f) => (
          <section className={styles.featureItem} key={f.title}>
            <Heading as="h3">{f.title}</Heading>
            <p>{f.description}</p>
          </section>
        ))}
      </div>
      <div className={styles.sidebarColumn}>
        <div className={styles.codeSnippet}>
          <div className={styles.codeSnippetHeader}>explicit_file.cx</div>
          <pre>
{`struct File : @nodrop {
    int fd;
};

void File::close(this) {
    @unsafe { close(this.fd); }
    @leak(this);
}

void process(File f) {
    if (f.fd < 0) {
        // Must handle here
        f.close();
        return;
    }

    // Explicit move
    worker(move f);
}`}
          </pre>
        </div>
        
        <div className={styles.quickLinksBox}>
          <Heading as="h4">QUICK LINKS</Heading>
          <ul>
            <li><Link to="/docs/manual/aggregate-types">Ownership Rules</Link></li>
            <li><Link to="/docs/manual/safe-functions">Safe Subset & Analysis</Link></li>
            <li><Link to="/docs/build-system">Build System (cx.toml)</Link></li>
          </ul>
        </div>
      </div>
    </div>
  );
}

export default function Home(): ReactNode {
  return (
    <Layout
      title="The CX Programming Language"
      description="A systems language for explicit resource management and formal verification.">
      <main className={styles.container}>
        <Hero />
        <MainContent />
      </main>
    </Layout>
  );
}
