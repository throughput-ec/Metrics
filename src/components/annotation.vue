<template>
  <div>
    <b-card title="Annotation infrastructure">
      <b-card
        title="Goal">
        <b-card-text>
          Improving the cyberinfrastructure required to maintain and serve an
          Annotation database for the Earth Sciences.
        </b-card-text>
      </b-card>
      <b-card title="Metrics">
        <b-card-text>
          <ul>
            <li>Development of a testing suite for the database and fully
              integrated development/production system.</li>
            <li>Clear documentation for online collaboration and protocols for
              bug reporting.</li>
            <li>Adoption and management of a Project Board system.
              [<a href="https://github.com/orgs/throughput-ec/projects">Throughput Project Boards</a>]</li>
            <li>Public database landing page with key reporting metrics (data
              volume, query speeds, etc.)</li>
          </ul>
        </b-card-text>
      </b-card>
    </b-card>
  </div>
</template>

<script>

  export default {
    name: 'annotationVue',
    props: {
      msg: String
    }
  }

</script>
