
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgetsqrhrt = require( './zgetsqrhrt.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgetsqrhrt, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgetsqrhrt;
