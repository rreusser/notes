
/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zla_syrcond_x = require( './zla_syrcond_x.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zla_syrcond_x, 'ndarray', ndarray );


// EXPORTS //

module.exports = zla_syrcond_x;
