import {useState, type ReactNode} from "react";
import Layout from "@theme-original/DocRoot/Layout";
import type LayoutType from "@theme/DocRoot/Layout";
import type {WrapperProps} from "@docusaurus/types";

import Foliage from "../../../components/Foliage";
import {StdlibOutlineContext} from "../../../lib/stdlib-outline";

type Props = WrapperProps<typeof LayoutType>;

export default function LayoutWrapper(props: Props): ReactNode {
    const [collapsed, setCollapsed] = useState(false);

    return (
        <StdlibOutlineContext.Provider value={{collapsed, setCollapsed}}>
            <Foliage />
            <Layout {...props} />
        </StdlibOutlineContext.Provider>
    );
}
