import { resolve } from 'path'
import { injectHtml } from 'vite-plugin-html'

const scalaVersion = '2.13'

export default ({ mode }) => {
  const mainJS = `/target/scala-${scalaVersion}/caliban-client-laminext-test-${mode === 'production' ? 'opt' : 'fastopt'}/main.js`
  return {
    publicDir: './static/public',
    plugins: [
      injectHtml({
        injectData: {
          injectScript: `<script type="module" src="${mainJS}"></script>`
        }
      })
    ]
  }
}