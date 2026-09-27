import type {ReactNode} from "react";
import Link from "@docusaurus/Link";

export default function NavbarLogo(): ReactNode {
    return (
        <Link className="navbar__brand cx-wordmark" to="/">
            <b>cx</b>
            <span className="cx-tag">Research preview</span>
        </Link>
    );
}
