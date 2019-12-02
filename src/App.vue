
<template>
  <div id="app"  class="mainpage">
    <Header />
    <parentPage />
  </div>
</template>

<script>

import './assets/text.css';
import './assets/containers.css'
import 'bootstrap/dist/css/bootstrap.css'
import 'bootstrap-vue/dist/bootstrap-vue.css'

import parentPage from './components/parentPage.vue'
import header from './components/header.vue'

export default {
  name: 'app',
  components: {
    'parentPage': parentPage,
    'Header': header,
  }
}
</script>
