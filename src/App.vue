<template>
  <div id="app"  class="mainpage">
    <img alt="Throughput logo" src="./assets/throughput.svg">
    <parentPage/>
  </div>
</template>

<script>

import './assets/text.css';
import './assets/containers.css'

import parentPage from './components/parentPage.vue'

export default {
  name: 'app',
  components: {
    'parentPage': parentPage,
  }
}
</script>
