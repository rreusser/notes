'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dormql = require( './dormql.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dormql, 'ndarray', ndarray );


// EXPORTS //

module.exports = dormql;
