'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zunmql = require( './zunmql.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zunmql, 'ndarray', ndarray );


// EXPORTS //

module.exports = zunmql;
