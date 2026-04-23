/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaunhr_col_getrfnp = require( './zlaunhr_col_getrfnp.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaunhr_col_getrfnp, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaunhr_col_getrfnp;
