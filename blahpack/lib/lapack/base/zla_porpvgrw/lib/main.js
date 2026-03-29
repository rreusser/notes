/* eslint-disable camelcase */

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zla_porpvgrw = require( './zla_porpvgrw.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zla_porpvgrw, 'ndarray', ndarray );


// EXPORTS //

module.exports = zla_porpvgrw;
