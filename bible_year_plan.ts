/// <reference types="./lib.deno.d.ts" />

/**
 * Insert 365 days of a Bible Reading Plan into ReadWise Reader
 *
 * ```
 * $ TOKEN="<...>" deno run --allow-env --allow-net bible_year_plan.ts
 * ```
 *
 * Generates 365 Reader items (one for each day) from the Bible.com reading plan
 * and saves them directly to ReadWise Reader.
 *
 * The script uses the day one URL pattern to generate all 365 days:
 * - Day 1: https://www.bible.com/reading-plans/53242/day/1?segment=0
 * - Day 2: https://www.bible.com/reading-plans/53242/day/2?segment=0
 * - ... and so on
 *
 * Saves one link at a time and waits 5s before saving the next, since the
 * Reader API has a 20 request per minute limit.
 *
 * <https://readwise.io/reader_api/>
 *
 * The Readwise Access Token (<https://readwise.io/access_token>) should be
 * provided in a `TOKEN` env var.
 */

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

const BASE_URL = "https://www.bible.com/reading-plans/53242/day";

const saveToReader = (readerPayload: ReaderItem) =>
  Promise.resolve(readerPayload)
    .then((body) => JSON.stringify(body))
    .then((body) =>
      fetch(
        "https://readwise.io/api/v3/save/",
        {
          method: "POST",
          body,
          headers: {
            "Content-Type": "application/json",
            "Authorization": `Token ${Deno.env.get("TOKEN")}`,
          },
        },
      )
    )
    .then((res) => res.ok ? res.headers : Promise.reject(res.text()));

const wait = (s: number) => new Promise((res) => setTimeout(res, s * 1000));

const formatDuration = (ms: number) => {
  const s = ms / 1000;

  if (s < 60) {
    return `${Math.floor(ms / 1000)}s`;
  }

  const m = s / 60;

  const s_ = Math.floor(s % 60);
  const m_ = Math.floor(m);

  return s_ > 0 ? `${m_}m ${s_}s` : `${m_}m`;
};

const createReaderItem = (day: number): ReaderItem => ({
  url: `${BASE_URL}/${day}?segment=0`,
  title: `Bible In A Year - Day ${day}`,
  tags: [
    "Bible",
    "Bible In A Year",
    "Daily Devotional",
  ],
  location: "new",
  saved_using: "Bible Year Plan Importer",
});

const saveDay = async (item: ReaderItem): Promise<void> => {
  try {
    await saveToReader(item);
    console.log(`[Saved] ${item.title}`);
  } catch (err) {
    console.error(`Failed to save day ${item.title}:`, err);
    throw err;
  }
};

const saveAllDays = async () => {
  const first = 352;
  const last = 365;
  const total = last - first + 1;
  let count = 0;
  const started = Date.now();

  for (let day = first; day <= last; day++) {
    const item = createReaderItem(day);
    await saveDay(item);
    count = day;
    console.log(`[${count}/${total}] Saved: ${item.title}`);
    await wait(5);
  }

  const finished = Date.now();
  console.log(`Saved ${total} days (from day ${first} to ${last}) in ${formatDuration(finished - started)}`);
};

if (!Deno.env.get("TOKEN")) {
  console.error("Missing TOKEN");
  console.error("See <https://readwise.io/access_token>");
  Deno.exit(1);
}

saveAllDays()
  .catch((err) => {
    console.error("Failed to save reading plan");
    console.error(err);
    Deno.exit(1);
  });

