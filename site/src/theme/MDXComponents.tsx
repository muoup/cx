import {useContext, type ComponentProps, type ReactNode} from "react";
import MDXComponents from "@theme-original/MDXComponents";
import MDXHeading from "@theme/MDXComponents/Heading";

import {DocDescriptionContext} from "../lib/doc-description";

function Title(props: ComponentProps<"h1">): ReactNode {
    const description = useContext(DocDescriptionContext);

    return (
        <header className="chl-doc-header">
            <MDXHeading as="h1" {...props} />
            {description && <p className="chl-doc-description">{description}</p>}
        </header>
    );
}

export default {
    ...MDXComponents,
    h1: Title,
};
