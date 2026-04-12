/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zla_syamv = require( './zla_syamv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zla_syamv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zla_syamv;
