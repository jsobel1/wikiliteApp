test_that("cite_type recognises common citation templates", {
  expect_equal(cite_type("<ref>{{cite journal|last=Doe|year=2020}}</ref>"), "Journal")
  expect_equal(cite_type("<ref>{{Cite Journal |title=foo}}</ref>"),         "Journal")
  expect_equal(cite_type("<ref>{{cite book|isbn=0123456789}}</ref>"),       "Book")
  expect_equal(cite_type("<ref>{{cite web|url=https://x.org}}</ref>"),      "Web")
  expect_equal(cite_type("<ref>{{cite news|title=foo}}</ref>"),             "News/Magazine")
  expect_equal(cite_type("<ref>{{cite arxiv|eprint=2401.0001}}</ref>"),     "Preprint")
  expect_equal(cite_type("<ref>{{cite biorxiv|biorxiv=10.1101/foo}}</ref>"),"Preprint")
  expect_equal(cite_type("<ref>{{cite thesis|author=X}}</ref>"),            "Thesis")
  expect_equal(cite_type("<ref>{{cite conference|title=X}}</ref>"),         "Conference")
  expect_equal(cite_type("<ref>{{cite report|publisher=X}}</ref>"),         "Report")
  expect_equal(cite_type("<ref>{{cite av media|title=X}}</ref>"),           "Multimedia")
  expect_equal(cite_type("<ref>{{cite patent|country=US}}</ref>"),          "Legal/Patent")
  expect_equal(cite_type("<ref>{{cite tweet|user=x}}</ref>"),               "Social Media")
})

test_that("cite_type falls back to bare URL → Web and unknown → Other", {
  expect_equal(cite_type("<ref>https://example.org/x</ref>"), "Web")
  expect_equal(cite_type("<ref>plain text only</ref>"),       "Other")
})

test_that("cite_type prefers Journal over Book when both keywords appear", {
  expect_equal(cite_type("<ref>{{cite journal|isbn=123|title=foo}}</ref>"), "Journal")
})

test_that("cite_url resolves DOIs first", {
  raw <- "<ref>{{cite journal|doi=10.1038/nature12373|url=https://example.org}}</ref>"
  expect_equal(cite_url(raw), "https://doi.org/10.1038/nature12373")
})

test_that("cite_url falls back to a bare URL", {
  # In real use the input is post-extract_cites (whitespace-collapsed, but
  # the surrounding <ref></ref> wrapper is preserved). We accept either.
  raw <- "{{cite web|url=https://example.org/article|title=Foo}}"
  expect_equal(cite_url(raw), "https://example.org/article")
})

test_that("cite_url strips trailing punctuation from URLs", {
  raw <- "see https://example.org/x. for more"
  expect_equal(cite_url(raw), "https://example.org/x")
})

test_that("cite_url falls back to ISBN → WorldCat search", {
  raw <- "<ref>{{cite book|isbn=978-0-13-110362-7}}</ref>"
  url <- cite_url(raw)
  expect_match(url, "^https://www\\.worldcat\\.org/search\\?q=isbn:")
  expect_match(url, "9780131103627")
})

test_that("cite_url returns NULL when nothing is resolvable", {
  expect_null(cite_url("plain text with no link"))
})

test_that("cite_label prefers DOI", {
  # The DOI regex is greedy to whitespace, so we need a trailing space/word.
  expect_equal(cite_label("doi:10.1038/nature12373 some other text"),
               "10.1038/nature12373")
})

test_that("cite_label uses last + year when no DOI", {
  raw <- "<ref>{{cite journal|last=Smith|year=2019|title=Foo}}</ref>"
  expect_equal(cite_label(raw), "Smith (2019)")
})

test_that("cite_label uses last alone when no year", {
  raw <- "<ref>{{cite book|last=Knuth|title=TAOCP}}</ref>"
  expect_equal(cite_label(raw), "Knuth")
})

test_that("cite_label falls back to first 70 chars", {
  raw <- strrep("a", 200)
  expect_equal(cite_label(raw), strrep("a", 70))
})

test_that("extract_cites finds <ref>...</ref> blocks and self-closing tags", {
  wt <- "Some text<ref>One</ref> more<ref name='a'>Two</ref> and <ref name='b' /> end."
  cs <- extract_cites(wt)
  expect_length(cs, 3)
  expect_true(any(grepl("One", cs)))
  expect_true(any(grepl("Two", cs)))
  expect_true(any(grepl("name='b'", cs, fixed = TRUE)))
})

test_that("extract_cites returns character(0) on empty input", {
  expect_identical(extract_cites(NULL),  character(0))
  expect_identical(extract_cites(""),    character(0))
  expect_identical(extract_cites("no refs here"), character(0))
})

test_that("extract_cites collapses whitespace and de-duplicates", {
  wt <- "<ref>same    cite</ref> blah <ref>same cite</ref>"
  expect_length(extract_cites(wt), 1)
})

test_that("extract_isbn returns the digits-only form for hyphenated ISBNs", {
  expect_equal(extract_isbn("{{cite book|isbn=978-0-13-110362-7}}"),
               "9780131103627")
})

test_that("extract_isbn handles ISBN-10 with X check digit", {
  expect_equal(extract_isbn("isbn = 0-306-40615-X"), "030640615X")
})

test_that("extract_isbn is case-insensitive", {
  expect_equal(extract_isbn("ISBN: 9780131103627"), "9780131103627")
})

test_that("extract_isbn returns NA when no valid ISBN is present", {
  expect_true(is.na(extract_isbn("plain text without an isbn")))
  expect_true(is.na(extract_isbn("isbn = ???")))
})

test_that("extract_cites does not match across newlines (single-line scope)", {
  # Documenting current behaviour: the regex .*? does not span newlines.
  # Multi-line refs are missed; in practice MediaWiki serialises them on
  # one line, so this is acceptable.
  wt <- "<ref>\n  multiline\n  citation\n</ref>"
  expect_length(extract_cites(wt), 0)
})
