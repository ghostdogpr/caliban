import { defineConfig } from 'vitepress'

const description = "Functional GraphQL library for Scala"

export default defineConfig({
  base: '/caliban/',
  title: "Caliban",
  description,
  head: [
    ['link', { rel: 'icon', href: '/caliban/favicon.png' }],
    ['script', { defer: '', src: 'https://cloud.umami.is/script.js', 'data-website-id': '2ba71608-2c44-4547-804e-ed78f3a41ad1' }],
    // VitePress does not add `base` to head entries, so these URLs are absolute.
    ['meta', { property: 'og:type', content: 'website' }],
    ['meta', { property: 'og:url', content: 'https://ghostdogpr.github.io/caliban/' }],
    ['meta', { property: 'og:title', content: 'Caliban' }],
    ['meta', { property: 'og:description', content: description }],
    ['meta', { property: 'og:image', content: 'https://ghostdogpr.github.io/caliban/og.png' }],
    ['meta', { property: 'og:image:width', content: '1200' }],
    ['meta', { property: 'og:image:height', content: '630' }],
    ['meta', { property: 'og:image:alt', content: 'Caliban logo (pink GraphQL nodes joined in the shape of the Scala logo) above the title Caliban: Functional GraphQL library for Scala' }],
    ['meta', { name: 'twitter:card', content: 'summary_large_image' }]
  ],

  markdown: {
    // mdoc compiles `mdoc:invisible` blocks to check the examples, but readers should not see them.
    config: md => {
      const fence = md.renderer.rules.fence!
      md.renderer.rules.fence = (tokens, idx, ...rest) =>
        tokens[idx].info.includes('mdoc:invisible') ? '' : fence(tokens, idx, ...rest)
    }
  },

  themeConfig: {
    nav: [
      { text: 'Home', link: '/' },
      { text: 'Documentation', link: '/docs/' },
      { text: 'Resources', link: '/resources/' },
      { text: 'FAQ', link: '/faq/' },
      { text: 'About', link: '/about/' },
      { text: 'Scaladoc', link: 'https://javadoc.io/doc/com.github.ghostdogpr/caliban_3/' },
    ],

    logo: { light: '/caliban.svg', dark: '/caliban-dark.svg' },

    sidebar: [
      {
        text: 'Caliban Server',
        collapsed: false,
        items: [
          { text: 'Getting started', link: '/docs/' },
          { text: 'Schema generation', link: '/docs/schema' },
          { text: 'Code generation', link: '/docs/server-codegen' },
          { text: 'HTTP adapters', link: '/docs/adapters' },
          { text: 'Middleware', link: '/docs/middleware' },
          { text: 'Query optimization', link: '/docs/optimization' },
          { text: 'Interop with other libraries', link: '/docs/interop' },
          { text: 'Federation', link: '/docs/federation' },
          { text: 'Relay connections', link: '/docs/relay-connections' },
          { text: 'Schema reporting', link: '/docs/schema-reporting' },
        ]
      },
      {
        text: 'Caliban Gateway',
        collapsed: false,
        items: [
          { text: 'Getting started', link: '/docs/gateway' },
          { text: 'Subgraphs', link: '/docs/gateway/subgraphs' },
          { text: 'Query planning', link: '/docs/gateway/planning' },
          { text: 'Subscriptions', link: '/docs/gateway/subscriptions' },
          { text: 'Hooks', link: '/docs/gateway/hooks' },
          { text: 'Configuration', link: '/docs/gateway/configuration' },
        ]
      },
      {
        text: 'Caliban Client',
        collapsed: false,
        items: [
          { text: 'Getting started', link: '/docs/client' },
          { text: 'Code generation', link: '/docs/client-codegen' },
          { text: 'Laminext integration', link: '/docs/laminext' },
        ]
      },
      {
        text: 'Caliban Tools',
        collapsed: false,
        items: [
          { text: 'Getting started', link: '/docs/tools' },
          { text: 'Stitching', link: '/docs/stitching' },
          { text: 'Schema comparison', link: '/docs/schema-comparison' },
        ]
      },
      {
        text: 'Examples', link: '/docs/examples'
      },
      {
        text: 'Resources', link: '/resources/'
      },
      {
        text: 'FAQ', link: '/faq/'
      },
      {
        text: 'About', link: '/about/'
      }
    ],

    // The VuePress sidebar listed h2 and h3 headings, so the outline shows both levels.
    outline: [2, 3],

    socialLinks: [
      { icon: 'github', link: 'https://github.com/ghostdogpr/caliban' }
    ],

    search: {
      provider: 'local'
    }
  }
})
