/// <reference types="./lib.deno.d.ts" />

/**
 * Scrape Ellen G. White writings from egwwritings.org and convert to Reader format
 *
 * ```
 * $ deno run --allow-net --allow-read egw_writings_scrapper.ts <bookTag> <url>
 * ```
 *
 * Expects a book tag (e.g., "PP", "AA", "COL") and a URL to an EGW Writings
 * table of contents page (e.g., https://m.egwwritings.org/en/book/127/toc)
 * or a local HTML file.
 *
 * Parses the table of contents structure and extracts chapter information,
 * converting titles to a standardized format with proper chapter numbering.
 *
 * Outputs a JSON array of Reader items that can be saved to ReadWise Reader
 * using the `reader-importer.ts` script.
 *
 * ```ts
 * type ReaderItem = {
 *   url: string;
 *   title: string;
 *   tags: string[];
 *   location: 'new' | 'archive' | 'later' | 'feed';
 *   saved_using?: string;
 *   description?: string;
 *   author?: string;
 *   summary?: string;
 *   image_url?: string;
 * }
 * ```
 *
 * Title parsing examples:
 * - "Preface" -> "PP Preface"
 * - "Chapter 6—We Have Seen His Star" -> "PP 6: We Have Seen His Star"
 * - Sub-chapters get numbered like "PP 6.1: Sub-chapter Title"
 *
 * The script handles both remote URLs and local HTML files, automatically
 * detecting the source type based on the URL extension.
 */

import { DOMParser } from "https://deno.land/x/deno_dom/deno-dom-wasm.ts";

// Preface                           -> NaN
// Chapter 6—“We Have Seen His Star” -> 6
const extractChapterNumber = (title_: string) => {
  if (!/^Chapter /.test(title_)) {
    return NaN;
  }

  const title = title_.replace(/^Chapter /, "");
  const [chapter, _rest] = title.split("—");

  return parseInt(chapter, 10);
};

// Preface                                          -> DA Preface
// Chapter 6—“We Have Seen His Star”                -> DA 6: We Have Seen His Star
// “Judge not, that ye be not judged.”—Matthew 7:1. -> MB "Judge not, that ye be not judged."—Matthew 7:1.
const parseTitle = (book: string) => (title__: string) => {
  const title_ = title__.split("\n").map((l) => l.trim()).join(" ");

  if (!/^Chapter /.test(title_)) {
    return `${book} ${
      title_.replace(/“/g, '"').replace(/”/g, '"').replace(/’/g, "'")
    }`;
  }

  const title = title_.replace(/^Chapter /, "");
  const [chapter, rest] = title.split("—");

  const rest_ = rest.replace(/“/g, '"').replace(/”/g, '"').replace(/’/g, "'");
  return `${book} ${chapter}: ${rest_}`;
};

type Resource = {
  url: string;
  title: string;
};

const toReader = (
  bookTag: string,
) =>
(
  { url, title }: Resource,
): ReaderItem => ({
  url,
  title,
  tags: [
    bookTag,
    "Books",
    "Ellen G. White",
  ],
  location: "new",
  saved_using: "EGW Writings Importer",
});

type ReaderItem = {
  url: string;
  title: string;
  tags: string[];
  location: "new" | "archive" | "later" | "feed";
  saved_using?: string;
  description?: string;
  author?: string;
  summary?: string;
  image_url?: string;
};

const BASE_URL = "https://m.egwwritings.org";

const getHtml = async (url: string) =>
  url.endsWith(".html")
    ? Deno.readTextFile(url)
    : fetch(url).then((res) => res.text());

try {
  // Get bookTag and URL from command line arguments
  const args = Deno.args;
  if (args.length < 2) {
    console.error("Usage: deno run egw_writings_scrapper.ts <bookTag> <url>");
    console.error(
      "Example: deno run egw_writings_scrapper.ts PP https://m.egwwritings.org/en/book/127/toc",
    );
    console.error("Example: deno run egw_writings_scrapper.ts AA mb.html");
    Deno.exit(1);
  }

  const [bookTag, url] = args;

  const html = await getHtml(url);

  const document = new DOMParser().parseFromString(html, "text/html");

  const links = [...document.querySelectorAll(".container > .toc > li")]
    .flatMap((li) => {
      if (li.classList.contains("has-children")) {
        const a = li.querySelector("a");

        const title = a.textContent;
        const href = a.getAttribute("href");

        const num = extractChapterNumber(a.textContent);

        const inner = [...li.querySelectorAll("ul > ul > li > a")]
          .map((a, i) => {
            const title = parseTitle(`${bookTag} ${num}.${i + 1}:`)(
              a.textContent,
            );
            const href = a.getAttribute("href");

            return { title, url: `${BASE_URL}${href}` };
          })
          .map(toReader(bookTag));

        return [
          { title: parseTitle(bookTag)(title), url: `${BASE_URL}${href}` },
          ...inner,
        ];
      }

      const a = li.querySelector("a");

      const title = a.textContent;
      const href = a.getAttribute("href");

      return [{ title: parseTitle(bookTag)(title), url: `${BASE_URL}${href}` }];
    })
    .map(toReader(bookTag));

  console.log(JSON.stringify(links, null, 2));
} catch (error) {
  console.log(error);
}
