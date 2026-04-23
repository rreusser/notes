
/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zla_porfsx_extended = require( './zla_porfsx_extended.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zla_porfsx_extended, 'ndarray', ndarray );


// EXPORTS //

module.exports = zla_porfsx_extended;
