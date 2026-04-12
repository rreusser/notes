/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zla_heamv = require( './zla_heamv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zla_heamv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zla_heamv;
