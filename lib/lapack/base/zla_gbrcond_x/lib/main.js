
/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zla_gbrcond_x = require( './zla_gbrcond_x.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zla_gbrcond_x, 'ndarray', ndarray );


// EXPORTS //

module.exports = zla_gbrcond_x;
