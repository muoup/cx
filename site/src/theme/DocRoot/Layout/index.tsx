import type {ReactNode} from "react";
import Layout from "@theme-original/DocRoot/Layout";
import type LayoutType from "@theme/DocRoot/Layout";
import type {WrapperProps} from "@docusaurus/types";

import Foliage from "../../../components/Foliage";

type Props = WrapperProps<typeof LayoutType>;

export default function LayoutWrapper(props: Props): ReactNode {
    return (
        <>
            <Foliage />
            <Layout {...props} />
        </>
    );
}
