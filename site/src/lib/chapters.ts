// Manual sidebar labels carry the chapter number: "7. Linear Resources".
export function chapterLabel(label: string): {number: number; title: string} | undefined {
    const match = label.match(/^(\d+)\.\s+(.*)$/);
    return match ? {number: Number(match[1]), title: match[2]} : undefined;
}
