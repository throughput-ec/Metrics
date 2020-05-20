<template>
  <div>
    <b-card title="Workflow Discovery">
      <b-card title="Goal">
        <b-card-text>
          Establishing a platform to support the addition of workflow elements
          to the TAE and to link primary data resources in the geosciences
          through publicly available code.
        </b-card-text>
        <b-card title="Metrics">
          <b-card-text>
            <ul>
              <li>Number of scripts recovered by programmatic workflows</li>
              <li>Number of user-submitted scripts</li>
              <li>Number of data resources connected through scripts</li>
              <li>Number of users accessing scripts through the system</li>
              <li>Number of workshops (and number of participants) that use annotation as an element in instruction.</li>
            </ul>
          </b-card-text>
        </b-card>
      </b-card>
    </b-card>
  </div>
</template>


<script>

  export default {
    name: 'ECcook',
    props: {
      msg: String
    }
  }

</script>
