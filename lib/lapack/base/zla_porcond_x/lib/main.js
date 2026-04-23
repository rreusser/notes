/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zla_porcond_x = require( './zla_porcond_x.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zla_porcond_x, 'ndarray', ndarray );


// EXPORTS //

module.exports = zla_porcond_x;
