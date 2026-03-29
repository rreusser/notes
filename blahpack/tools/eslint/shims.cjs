'use strict';

// Drop-in replacements for @stdlib utilities used by ESLint rules.
// Each export matches the API of the corresponding @stdlib package.

var builtins = require('module').builtinModules || [];

exports.contains = function contains( arr, val ) {
	if ( typeof arr === 'string' ) {
		return arr.indexOf( val ) !== -1;
	}
	return arr.indexOf( val ) !== -1;
};

exports.startsWith = function startsWith( str, prefix, pos ) {
	if ( typeof pos === 'number' ) {
		return str.startsWith( prefix, pos );
	}
	return str.startsWith( prefix );
};

exports.endsWith = function endsWith( str, suffix, len ) {
	if ( typeof len === 'number' ) {
		return str.endsWith( suffix, len );
	}
	return str.endsWith( suffix );
};

exports.isArray = Array.isArray;

exports.isObject = function isObject( val ) {
	return val !== null && typeof val === 'object' && !Array.isArray( val );
};

exports.isString = function isString( val ) {
	return typeof val === 'string';
};
exports.isString.isPrimitive = function isPrimitiveString( val ) {
	return typeof val === 'string';
};

exports.isCapitalized = function isCapitalized( str ) {
	if ( typeof str !== 'string' || str.length === 0 ) {
		return false;
	}
	return str.charAt( 0 ) === str.charAt( 0 ).toUpperCase() &&
		str.charAt( 0 ) !== str.charAt( 0 ).toLowerCase();
};

exports.isUppercase = function isUppercase( str ) {
	return typeof str === 'string' && str === str.toUpperCase() && str !== str.toLowerCase();
};

exports.isNodeBuiltin = function isNodeBuiltin( name ) {
	return builtins.indexOf( name ) !== -1;
};

exports.trim = function trim( str ) {
	return str.trim();
};

exports.ltrim = function ltrim( str ) {
	return str.replace( /^\s+/, '' );
};

exports.uppercase = function uppercase( str ) {
	return str.toUpperCase();
};

exports.indexOf = function indexOf( arr, val ) {
	return arr.indexOf( val );
};

exports.copy = function copy( val ) {
	return JSON.parse( JSON.stringify( val ) );
};

exports.hasOwnProp = function hasOwnProp( obj, prop ) {
	return Object.prototype.hasOwnProperty.call( obj, prop );
};

exports.reFromString = function reFromString( str ) {
	var match = str.match( /^\/(.+)\/([gimsuy]*)$/ );
	if ( match ) {
		return new RegExp( match[1], match[2] );
	}
	return null;
};

exports.isRegExpString = function isRegExpString( str ) {
	return typeof str === 'string' && /^\/(.+)\/([gimsuy]*)$/.test( str );
};

exports.replace = function replace( str, search, replacement ) {
	return str.replace( search, replacement );
};

exports.objectKeys = Object.keys;
