
/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zla_gerfsx_extended = require( './zla_gerfsx_extended.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zla_gerfsx_extended, 'ndarray', ndarray );


// EXPORTS //

module.exports = zla_gerfsx_extended;
