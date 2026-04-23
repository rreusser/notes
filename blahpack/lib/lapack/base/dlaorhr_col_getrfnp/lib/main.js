/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaorhr_col_getrfnp = require( './dlaorhr_col_getrfnp.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaorhr_col_getrfnp, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaorhr_col_getrfnp;
