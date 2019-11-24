<template>
  <div>
    <div class="componentbox">
      <h2>Annotation infrastructure</h2>
      <div class="componentbox">
        <h3>GOAL</h3>
        <ul>
          <li>Improving the cyberinfrastructure required to maintain and serve an Annotation database for the Earth Sciences.</li>
        </ul>
      </div>
      <div class="componentbox">
        <h3>METRICS</h3>
        <ul>
          <li>Development of a testing suite for the database and fully integrated development/production system.</li>
          <li>Clear documentation for online collaboration and protocols for bug reporting.</li>
          <met-collab />
          <li>Adoption and management of a Project Board system. [<a href="http://github.com/throughput-ec">Throughput Project Boards</a>]</li>
          <li>Public database landing page with key reporting metrics (data volume, query speeds, etc.)</li>
        </ul>
      </div>
      <h3>GOAL</h3>
      Building user interfaces to support user groups within the earth sciences including:

      <h4>Direct annotation</h4>
      <h5>The Earth Science Cookbook.</h5>
      <h6>METRICS</h6>
      <ul>
        <li>Direct annotation landing page built and tagged in a version control system (GitHub), with Usability report from the UMN Usability Lab.</li>
        <li>Documentation surrounding the usability report.</li>
        <li>Earth Science Cookbook UI built and tagged in version control system.</li>
        <li>Usability report from the Usability Lab.</li>
      </ul>

      <h5>Workflow Discovery</h5>
      <h6>GOAL</h6>
      Establishing a platform to support the addition of workflow elements to the TAE and to link primary data resources in the geosciences through publicly available code.

      <h6>METRICS</h6>
      <ul>
        <li>Number of scripts recovered by programmatic workflows</li>
        <li>Number of user-submitted scripts</li>
        <li>Number of data resources connected through scripts</li>
        <li>Number of users accessing scripts through the system</li>
        <li>Number of workshops (and number of participants) that use annotation as an element in instruction.</li>
      </ul>
    </div>
  </div>
</template>


<script>

  import collaborationVue from './collab.vue'

  export default {
    name: 'annotationVue',
    props: {
      msg: String
    },
    components: {
      'met-collab': collaborationVue,
    }
  }

</script>
