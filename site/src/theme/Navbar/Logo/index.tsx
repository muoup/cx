import type {ReactNode} from "react";
import Link from "@docusaurus/Link";

export default function NavbarLogo(): ReactNode {
    return (
        <Link className="navbar__brand chl-wordmark" to="/">
            <b>Chloro</b>
            {/*<span className="chl-tag">Research preview</span>*/}
        </Link>
    );
}
