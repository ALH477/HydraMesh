# Third-party notices — `GUI/Punctim-Comms-Review.html`

`Punctim-Comms-Review.html` is a single-file bundle: a small unpacker, a page template, and a
manifest of base64-encoded assets that the unpacker turns into blob URLs at load time. Nothing is
fetched from a CDN. The template, styles, design-system components and the three comms scripts
(mesh simulation, views, app shell) are DeMoD LLC's, licensed `LGPL-3.0-only` like the rest of
the tree. Everything else embedded in the file is third-party and is listed here.

How this list was established: the manifest was decoded and each asset identified. The three
scripts are byte-identical to the files npm publishes —
`react@18.3.1/umd/react.development.js`, `react-dom@18.3.1/umd/react-dom.development.js` and
`@babel/standalone@7.29.0/babel.min.js` — and their SHA-384 digests match the `integrity`
attributes in the template. The fonts were identified from their own `name` tables.

| Component | Version | Licence | Copyright |
|---|---|---|---|
| React (`react.development.js`) | 18.3.1 | MIT | Copyright (c) Facebook, Inc. and its affiliates. |
| ReactDOM (`react-dom.development.js`, includes the `scheduler` package) | 18.3.1 | MIT | Copyright (c) Facebook, Inc. and its affiliates. |
| Babel standalone (`babel.min.js`) | 7.29.0 | MIT, bundling the packages listed below | Copyright (c) 2014-present Sebastian McKenzie and other contributors |
| Inter, weights 400-800, 7 woff2 subsets | 4.001 | OFL-1.1 | Copyright 2016 The Inter Project Authors (https://github.com/rsms/inter) |
| JetBrains Mono, weights 400-800, 6 woff2 subsets | 2.211 | OFL-1.1 | Copyright 2020 The JetBrains Mono Project Authors (https://github.com/JetBrains/JetBrainsMono) |

## React and ReactDOM — MIT

ReactDOM's build carries a code comment crediting a custom Modernizr build
("`@license Modernizr 3.0.0pre (Custom Build) | MIT`"); it is part of React's own distribution,
under React's licence below.

```text
MIT License

Copyright (c) Facebook, Inc. and its affiliates.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

## Babel standalone — MIT

```text
MIT License

Copyright (c) 2014-present Sebastian McKenzie and other contributors

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
```

### Packages bundled inside Babel standalone

`@babel/standalone` is a single minified build of Babel and its npm dependencies. The bundled
packages were read from the `sources` list of the source map npm publishes with it
(`babel.min.js.map`); the copyright lines are those in each package's own licence file on npm.
Years in those lines can differ slightly from the exact versions Babel bundled.

| Package | Licence | Copyright |
|---|---|---|
| @babel/* (the Babel packages, including regenerator-runtime in @babel/helpers) | MIT | Copyright (c) 2014-present Sebastian McKenzie and other contributors; the regenerator-runtime helper also carries "Copyright (c) 2014-present, Facebook, Inc." |
| @jridgewell/gen-mapping, remapping, sourcemap-codec, trace-mapping | MIT | Copyright 2024 Justin Ridgewell <justin@ridgewell.name> |
| @jridgewell/resolve-uri | MIT | Copyright 2019 Justin Ridgewell <jridgewell@google.com> |
| babel-plugin-polyfill-corejs2, -corejs3, -regenerator | MIT | Copyright (c) 2014-present Nicolò Ribaudo and other contributors |
| browserslist | MIT | Copyright 2014 Andrey Sitnik <andrey@sitnik.ru> and other contributors |
| caniuse-lite (browser-usage data) | CC-BY-4.0 | Data from caniuse.com by Alexis Deveria (caniuse-db); packaged as caniuse-lite by Ben Briggs |
| convert-source-map | MIT | Copyright 2013 Thorsten Lorenz. |
| core-js-compat | MIT | Copyright (c) 2014-2025 Denis Pushkarev |
| debug | MIT | Copyright (c) 2014-2017 TJ Holowaychuk <tj@vision-media.ca>; Copyright (c) 2018-2021 Josh Junon |
| electron-to-chromium (version table) | ISC | Copyright 2018 Kilian Valkhof |
| gensync | MIT | Copyright 2018 Logan Smyth <loganfsmyth@gmail.com> |
| js-tokens | MIT | Copyright (c) 2014, 2015, 2016, 2017, 2018 Simon Lydell |
| jsesc, regenerate, regenerate-unicode-properties, regexpu-core, unicode-canonical-property-names-ecmascript, unicode-match-property-ecmascript, unicode-match-property-value-ecmascript, unicode-property-aliases-ecmascript | MIT | Copyright Mathias Bynens <https://mathiasbynens.be/> |
| lru-cache, yallist, semver | ISC | Copyright (c) Isaac Z. Schlueter and Contributors |
| ms | MIT | Copyright (c) 2020 Vercel, Inc. |
| picocolors | ISC | Copyright (c) 2021-2024 Oleksii Raspopov, Kostiantyn Denysov, Anton Verinov |
| regjsgen | MIT | Copyright 2014-2020 Benjamin Tan <https://ofcr.se/> |
| regjsparser | BSD-2-Clause | Copyright (c) Julian Viereck and Contributors, All Rights Reserved. |

**MIT.** For every package above marked MIT, the copyright line(s) above apply together with
this permission notice (the MIT terms, worded as in React's licence above):

```text
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

**ISC.** For every package above marked ISC, the copyright line above applies together with
this notice (reproduced here from `semver`):

```text
The ISC License

Copyright (c) Isaac Z. Schlueter and Contributors

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
```

**BSD-2-Clause** (`regjsparser`):

```text
Copyright (c) Julian Viereck and Contributors, All Rights Reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
  * Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```

**CC-BY-4.0** (`caniuse-lite`). The bundle includes caniuse-lite's packed browser-agent table
(`data/agents.js`, `browsers.js`, `browserVersions.js`): browser usage data from caniuse.com by
Alexis Deveria, packaged as caniuse-lite by Ben Briggs, licensed under the Creative Commons
Attribution 4.0 International licence (https://creativecommons.org/licenses/by/4.0/; full text
in [`../LICENSES/CC-BY-4.0.txt`](../LICENSES/CC-BY-4.0.txt)). caniuse-lite stores it in a
compacted form, and the Babel build minified it; no other change was made here. It is provided
as-is, without warranties.

## Inter and JetBrains Mono — SIL Open Font License 1.1

The fonts are embedded as woff2 subsets split by Unicode range. The \`@font-face\` rules carry
Google Fonts' per-subset comments and \`unicode-range\` splits, so they appear to be the subsets
Google Fonts serves. They are used only to render this page.

```text
Copyright 2016 The Inter Project Authors (https://github.com/rsms/inter)
Copyright 2020 The JetBrains Mono Project Authors (https://github.com/JetBrains/JetBrainsMono)

This Font Software is licensed under the SIL Open Font License, Version 1.1.
This license is copied below, and is also available with a FAQ at:
https://openfontlicense.org

SIL OPEN FONT LICENSE

Version 1.1 - 26 February 2007

PREAMBLE

The goals of the Open Font License (OFL) are to stimulate worldwide development of collaborative font projects, to support the font creation efforts of academic and linguistic communities, and to provide a free and open framework in which fonts may be shared and improved in partnership with others.

The OFL allows the licensed fonts to be used, studied, modified and redistributed freely as long as they are not sold by themselves. The fonts, including any derivative works, can be bundled, embedded, redistributed and/or sold with any software provided that any reserved names are not used by derivative works. The fonts and derivatives, however, cannot be released under any other type of license. The requirement for fonts to remain under this license does not apply to any document created using the fonts or their derivatives.

DEFINITIONS

"Font Software" refers to the set of files released by the Copyright Holder(s) under this license and clearly marked as such. This may include source files, build scripts and documentation.

"Reserved Font Name" refers to any names specified as such after the copyright statement(s).

"Original Version" refers to the collection of Font Software components as distributed by the Copyright Holder(s).

"Modified Version" refers to any derivative made by adding to, deleting, or substituting — in part or in whole — any of the components of the Original Version, by changing formats or by porting the Font Software to a new environment.

"Author" refers to any designer, engineer, programmer, technical writer or other person who contributed to the Font Software.

PERMISSION & CONDITIONS

Permission is hereby granted, free of charge, to any person obtaining a copy of the Font Software, to use, study, copy, merge, embed, modify, redistribute, and sell modified and unmodified copies of the Font Software, subject to the following conditions:

1) Neither the Font Software nor any of its individual components, in Original or Modified Versions, may be sold by itself.

2) Original or Modified Versions of the Font Software may be bundled, redistributed and/or sold with any software, provided that each copy contains the above copyright notice and this license. These can be included either as stand-alone text files, human-readable headers or in the appropriate machine-readable metadata fields within text or binary files as long as those fields can be easily viewed by the user.

3) No Modified Version of the Font Software may use the Reserved Font Name(s) unless explicit written permission is granted by the corresponding Copyright Holder. This restriction only applies to the primary font name as presented to the users.

4) The name(s) of the Copyright Holder(s) or the Author(s) of the Font Software shall not be used to promote, endorse or advertise any Modified Version, except to acknowledge the contribution(s) of the Copyright Holder(s) and the Author(s) or with their explicit written permission.

5) The Font Software, modified or unmodified, in part or in whole, must be distributed entirely under this license, and must not be distributed under any other license. The requirement for fonts to remain under this license does not apply to any document created using the Font Software.

TERMINATION

This license becomes null and void if any of the above conditions are not met.

DISCLAIMER

THE FONT SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO ANY WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT OF COPYRIGHT, PATENT, TRADEMARK, OR OTHER RIGHT. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, INCLUDING ANY GENERAL, SPECIAL, INDIRECT, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF THE USE OR INABILITY TO USE THE FONT SOFTWARE OR FROM OTHER DEALINGS IN THE FONT SOFTWARE.
```
