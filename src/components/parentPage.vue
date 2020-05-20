<template>
  <div class="hello">
    <h1>Throughput Metrics</h1>
    This project is to be evaluated using Project Metrics defined in the
    Throughput project proposal
      <b-button variant="primary"
                size="sm"
                href="https://nsf.gov/awardsearch/showAward?AWD_ID=1928366">
          NSF-1928366
      </b-button>.

    <br>
    <met-anno />
    <br>
    <met-collab />
    <br>
    <ec-cook />
    <br>
    <cross-dis />
  </div>
</template>

<script>
import annotationVue from './annotation.vue'
import collaborationVue from './collab.vue'
import ECcook from './earthscicook.vue'
import crossDiscip from './crossdiscip.vue'

export default {
  name: 'parentPage',
  props: {
    msg: String
  },
  components: {
      'met-anno': annotationVue,
    'met-collab': collaborationVue,
    'ec-cook': ECcook,
    'cross-dis': crossDiscip,
  }
}

</script>
