module.exports = {
  base: '/caliban/',
  head: [
    ['link', { rel: 'icon', href: '/caliban.png' }]
  ],
  locales: {
    '/': {
      lang: 'en-US',
      title: 'Caliban',
      description: 'Functional GraphQL backend in Scala',
    }
  },
  themeConfig: {
    logo: '/caliban.svg',
    locales: {
      '/': {
        selectText: 'Language',
        label: 'English',
        nav: [
          { text: 'Documentation', link: '/docs/' },
          { text: 'About', link: '/about/' },
          { text: 'Github', link: 'https://github.com/ghostdogpr/caliban' },
          { text: 'Scaladoc', link: 'https://javadoc.io/doc/com.github.ghostdogpr/caliban_2.12/' },
        ],
        sidebar: {
          '/docs/': [
            {
              title: 'Caliban',
              collapsable: false,
              sidebarDepth: 2,
              children: [
                '',
                'schema',
                // 'optimization', uncomment when ready to be published
                'validation',
                'introspection',
                'interop',
                'examples'
              ]
            }
          ]
        }
      }
    },
  }
}