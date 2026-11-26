import {createContext} from "react";

type StdlibOutlineState = {
    collapsed: boolean;
    setCollapsed: (collapsed: boolean) => void;
};

export const StdlibOutlineContext = createContext<StdlibOutlineState | null>(null);
