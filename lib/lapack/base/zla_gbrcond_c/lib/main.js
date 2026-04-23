
/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zla_gbrcond_c = require( './zla_gbrcond_c.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zla_gbrcond_c, 'ndarray', ndarray );


// EXPORTS //

module.exports = zla_gbrcond_c;
