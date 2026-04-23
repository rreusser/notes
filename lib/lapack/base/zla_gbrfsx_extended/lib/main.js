/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zla_gbrfsx_extended = require( './zla_gbrfsx_extended.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zla_gbrfsx_extended, 'ndarray', ndarray );


// EXPORTS //

module.exports = zla_gbrfsx_extended;
