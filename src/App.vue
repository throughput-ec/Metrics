<template>
  <div id="app"  class="mainpage">
    <img alt="Throughput logo" src="./assets/throughput.svg">
    <pPage msg="Welcome to Your Vue.js App"/>
  </div>
</template>

<script>

import './assets/text.css';
import './assets/containers.css'

import pPage from './components/parentPage.vue'

export default {
  name: 'app',
  components: {
    pPage
  }
}
</script>
