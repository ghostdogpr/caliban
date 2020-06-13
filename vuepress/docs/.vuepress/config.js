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
            title: 'Caliban',
            collapsable: false,
            sidebarDepth: 2,
            children: [
              '',
              'schema',
              'middleware',
              'optimization',
              'validation',
              'introspection',
              'interop',
              'client',
              'federation',
              'tools',
              'examples',
            ]
          }]
        }
      }
    },
  }
}