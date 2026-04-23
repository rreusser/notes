/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaunhr_col_getrfnp2 = require( './zlaunhr_col_getrfnp2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaunhr_col_getrfnp2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaunhr_col_getrfnp2;
