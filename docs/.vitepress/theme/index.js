import { h } from 'vue'
import DefaultTheme from 'vitepress/theme'
import './custom.css'

export default {
  extends: DefaultTheme,
  Layout: () =>
    h(DefaultTheme.Layout, null, {
      'home-hero-info-before': () =>
        h(
          'a',
          { class: 'stack-badge', href: 'https://ghostdogpr.github.io/', target: '_blank', rel: 'noreferrer' },
          'Part of the ghostdogpr stack →',
        ),
    }),
}
