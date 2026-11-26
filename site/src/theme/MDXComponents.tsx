import type {ComponentProps, ReactNode} from "react";
import MDXComponents from "@theme-original/MDXComponents";
import MDXHeading from "@theme/MDXComponents/Heading";

function Title(props: ComponentProps<"h1">): ReactNode {
    return (
        <header className="chl-doc-header">
            <MDXHeading as="h1" {...props} />
        </header>
    );
}

export default {
    ...MDXComponents,
    h1: Title,
};
