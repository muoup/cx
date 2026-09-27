import {createContext} from "react";

// Set by DocItem/Layout so the MDX h1 can render the front-matter description beneath it.
export const DocDescriptionContext = createContext<string | undefined>(undefined);
