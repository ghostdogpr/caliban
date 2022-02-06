module.exports = {
  base: '/caliban/',
  head: [
    ['link', {
      rel: 'icon',
      href: '/caliban.png'
    }]
  ],
  locales: {
    '/': {
      lang: 'en-US',
      title: 'Caliban',
      description: 'Functional GraphQL library for Scala',
    }
  },
  themeConfig: {
    logo: '/caliban.svg',
    locales: {
      '/': {
        selectText: 'Language',
        label: 'English',
        nav: [{
            text: 'Documentation',
            link: '/docs/'
          },
          {
            text: 'Resources',
            link: '/resources/'
          },
          {
            text: 'FAQ',
            link: '/faq/'
          },
          {
            text: 'About',
            link: '/about/'
          },
          {
            text: 'Github',
            link: 'https://github.com/ghostdogpr/caliban'
          },
          {
            text: 'Scaladoc',
            link: 'https://javadoc.io/doc/com.github.ghostdogpr/caliban_2.12/'
          },
        ],
        sidebar: {
          '/docs/': [{
            title: 'Caliban Server',
            collapsable: true,
            sidebarDepth: 2,
            children: [
              '',
              'schema',
              'middleware',
              'optimization',
              'validation',
              'introspection',
              'adapters',
              'interop',
              'federation',
              'relay-connections',
              'schema-reporting'
            ]
          },
          {
            title: 'Caliban Client',
            collapsable: true,
            sidebarDepth: 2,
            children: [
              'client',
              'client-codegen',
              'laminext'
            ]
          },
          {
            title: 'Caliban Tools',
            collapsable: true,
            sidebarDepth: 2,
            children: [
              'tools',
              'stitching',
              'schema-comparison',
            ]
          },
          {
            title: 'Examples',
            path: 'examples'
          }]
        }
      }
    },
  }
}