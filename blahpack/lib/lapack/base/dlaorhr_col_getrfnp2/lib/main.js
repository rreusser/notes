
/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaorhr_col_getrfnp2 = require( './dlaorhr_col_getrfnp2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaorhr_col_getrfnp2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaorhr_col_getrfnp2;
