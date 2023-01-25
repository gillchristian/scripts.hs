/// <reference types="./lib.deno.d.ts" />

/**
 * Save links from Listas to ReadWise Reader
 *
 * ```
 * $ TOKEN="<...>" deno run --allow-env --allow-read --allow-write --allow-net listas-to-reader.ts
 * ```
 *
 * Expects the list of Listas links to be on `resources.pending.json` and a list
 * of lists in `lists.json`, the later is optional and is used to set the list
 * name and tags as tags of the links.
 *
 * Saves one link at a time and then waits 5s before saving the next, since the
 * Reader API has a 20 request per minute limit.
 *
 * On each save `resources.pending.json` is updated and the progress is saved to
 * `resources.saved.json`, this allows to continue if the process is
 * interrumpted.
 *
 * <https://readwise.io/reader_api/>
 *
 * To fetch the lists:
 *
 * ```
 * curl 'https://api.listas.io/lists' \
 *   -H 'Accept: application/json' \
 *   -H 'Authorization: Bearer <...>' \
 *   --compressed | jq '.' > lists.json
 * ```
 *
 * To fetch the resources:
 *
 * ```
 * curl 'https://api.listas.io/resources?completed=true' \
 *   -H 'Accept: application/json' \
 *   -H 'Authorization: Bearer <...>' \
 *   --compressed | jq '.' > resources.pending.json
 * ```
 *
 * The Readwise Access Token (<https://readwise.io/access_token>) should be
 * provided in a `TOKEN` env var.
 */
type List = {
  id: string;
  title: string;
  tags: string[];
};

type Resource = {
  url: string;
  list: string;
  tags: string[];
  title: string[];
  description?: string;
  completed_at?: string;
};

type Lists = { [key: string]: List };

const listasToReader = (
  lists: Lists,
  { list, url, tags, title, description, completed_at }: Resource,
) => ({
  url,
  title,
  tags: [
    ...new Set(
      [...tags, lists[list]?.title, ...lists[list]?.tags]
        .sort()
        .filter(Boolean)
        .map((t) => t.toLowerCase()),
    ),
  ],
  location: completed_at ? "archive" : "new",
  description,
  saved_using: "Listas",
});

const normalizeLists = (lists: List[]) =>
  lists.reduce(
    (acc, cur) => {
      acc[cur.id] = cur;

      return acc;
    },
    {} as Lists,
  );

const saveToReader = (lists: Lists, listasResource: Resource) =>
  Promise.resolve(listasToReader(lists, listasResource))
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

const savePending = (body: Resource[]) =>
  Deno.writeTextFile("resources.pending.json", JSON.stringify(body, null, 2));

const readPending = (): Promise<Resource[]> =>
  Deno.readTextFile("resources.pending.json")
    .then((content) => JSON.parse(content));

const saveSaved = (body: Resource[]) =>
  Deno.writeTextFile("resources.saved.json", JSON.stringify(body, null, 2));

const readSaved = (): Promise<Resource[]> =>
  Deno.readTextFile("resources.saved.json")
    .then((content) => JSON.parse(content));

const readLists = (): Promise<Lists> =>
  Deno.readTextFile("lists.json")
    .then((content) => JSON.parse(content))
    .then(normalizeLists)
    .catch(() => {
      console.error("No `lists.json` found");
      console.error("Will not use the lists names as tags for the links");

      return {};
    });

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

const saveNext = async (
  lists: Lists,
  count: number,
  [next, ...pending]: Resource[],
  saved: Resource[],
): Promise<[number, number]> => {
  if (!next) {
    return [Date.now(), count];
  }

  try {
    await saveToReader(lists, next);

    console.log(`[Saved] ${next.title}`);

    const saved_ = [...saved, next];

    await savePending(pending);
    await saveSaved(saved_);
    await wait(5);

    return saveNext(lists, count + 1, pending, saved_);
  } catch (err) {
    console.error("Failed to save", next.title);
    console.error(err);
    Deno.exit(1);
  }
};

if (!Deno.env.get("TOKEN")) {
  console.error("Missing TOKEN");
  console.error("See <https://readwise.io/access_token>");
  Deno.exit(1);
}

const started = Date.now();

Promise.all([readPending(), readSaved(), readLists()])
  .then(([pending, saved, lists]) => saveNext(lists, 0, pending, saved))
  .then(([finished, count]) => {
    console.log(
      `Saved ${count} links in ${formatDuration(finished - started)}`,
    );
  })
  .catch((err) => {
    console.error("Failed loop");
    console.error(err);
    Deno.exit(1);
  });
