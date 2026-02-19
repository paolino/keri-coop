import { hash } from "blake3/browser";

export const hashImpl = (bytes) => hash(bytes);
