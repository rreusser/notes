

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zgelqf = require( './zgelqf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgelqf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgelqf;
